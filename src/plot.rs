use std::fmt;
use std::fs::File;
use std::path::Path;
use std::str::FromStr;

use image::{ImageOutputFormat, Rgb, RgbImage};

pub struct View {
    pub xmin: f64,
    pub ymin: f64,
    pub width: f64,
    pub height: f64,
}

impl View {
    pub fn new(center: (f64, f64), width: f64, height: f64) -> Self {
        View {
            width,
            height,
            xmin: center.0 - width / 2.0,
            ymin: center.1 - height / 2.0,
        }
    }
}

pub enum SaveError {
    IOError(std::io::Error),
    ImageError(image::error::ImageError),
}

impl fmt::Display for SaveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            SaveError::IOError(err) => write!(f, "Error while opening file: {}", err),
            SaveError::ImageError(err) => write!(f, "Error while writing image: {}", err),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Color(Rgb<u8>);

impl FromStr for Color {
    type Err = String;
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        if let Some(s) = string.strip_prefix("rgb:") {
            match TryInto::<[&str; 3]>::try_into(s.split(',').collect::<Vec<&str>>()) {
                Ok([red, green, blue]) => match (red.parse(), green.parse(), blue.parse()) {
                    (Ok(r), Ok(g), Ok(b)) => Ok(Color(Rgb([r, g, b]))),
                    _ => Err(string.into()),
                },
                Err(_) => Err(string.into()),
            }
        } else {
            match string {
                "black" => Ok(Color(Rgb([0, 0, 0]))),
                "red" => Ok(Color(Rgb([255, 0, 0]))),
                "green" => Ok(Color(Rgb([0, 255, 0]))),
                "blue" => Ok(Color(Rgb([0, 0, 255]))),
                "cyan" => Ok(Color(Rgb([0, 255, 255]))),
                "magenta" => Ok(Color(Rgb([255, 0, 255]))),
                "yellow" => Ok(Color(Rgb([255, 255, 0]))),
                "white" => Ok(Color(Rgb([255, 255, 255]))),
                _ => Err(string.into()),
            }
        }
    }
}

pub struct Plot {
    view: View,
    image: RgbImage,
}

impl Plot {
    pub fn new(view: View, cols: u32, rows: u32, color: Color) -> Self {
        Plot {
            view,
            image: RgbImage::from_pixel(cols, rows, color.0),
        }
    }

    fn put_pixel(&mut self, pt: (i32, i32), color: Color) -> bool {
        let (width, height) = self.image.dimensions();
        if 0 <= pt.0 && (pt.0 as u32) < width && 0 <= pt.1 && (pt.1 as u32) < height {
            self.image.put_pixel(pt.0 as u32, pt.1 as u32, color.0);
            true
        } else {
            false
        }
    }

    fn coords_to_pixel(&self, coord: (f64, f64)) -> (i32, i32) {
        let c = (coord.0 - self.view.xmin) / self.view.width;
        let r = 1.0 - (coord.1 - self.view.ymin) / self.view.height;
        let (c, r) = (
            c * (self.image.width() as f64),
            r * (self.image.height() as f64),
        );
        (c.floor() as i32, r.floor() as i32)
    }

    // Use Bresenham's algorithm with modification
    pub fn draw_line(&mut self, a: (f64, f64), b: (f64, f64), thickness: f64, color: Color) {
        // Convert to pixel coordinates
        let (mut a, mut b) = (self.coords_to_pixel(a), self.coords_to_pixel(b));

        let (mut dx, mut dy) = (b.0 - a.0, b.1 - a.1);
        let steep = dy.abs() > dx.abs();
        if steep {
            // We swap x and y so the slope is less than one
            (dx, dy) = (dy, dx);
            (a.0, a.1) = (a.1, a.0);
            (b.0, b.1) = (b.1, b.0);
        }

        // Make sure `b` is to the right of `a` so `dx > 0`
        if b.0 < a.0 {
            (a, b) = (b, a);
            (dx, dy) = (-dx, -dy);
        }

        // Calculate vertical thickness of line
        let secant = f64::hypot(dx as f64, dy as f64) / (dx.max(1) as f64);
        let vert_range = (thickness * secant).abs().floor() as i32;

        // Track current midline pixel and error
        let mut mid_y: i32 = a.1;
        // If f(x) defines the line then
        //   error = (f(x) - y) * dx
        let mut error: i32 = 0;
        for x in a.0..b.0 + 1 {
            for y in mid_y - vert_range..mid_y + vert_range + 1 {
                if steep {
                    self.put_pixel((y, x), color);
                } else {
                    self.put_pixel((x, y), color);
                }
            }

            if error >= dx {
                mid_y += 1;
                error -= dx;
            } else if error < 0 {
                mid_y -= 1;
                error += dx;
            }
            error += dy;
        }
    }

    pub fn draw_gridlines(&mut self, vert_divs: u8, horiz_divs: u8, thickness: f64, color: Color) {
        let rectify_size = |size: f64| {
            let lg2 = (2.0_f64).log10();
            let lg5 = (5.0_f64).log10();

            let log = size.log10();
            let frac = log - log.floor();
            let int_part = (10.0_f64).powf(log.floor());
            if frac < lg2 {
                2.0 * int_part
            } else if frac < lg5 {
                5.0 * int_part
            } else {
                10.0 * int_part
            }
        };

        let (xmin, ymin) = (self.view.xmin, self.view.ymin);
        let (xmax, ymax) = (xmin + self.view.width, ymin + self.view.height);

        let dx = rectify_size(self.view.width / horiz_divs as f64);
        let mut x = (xmin / dx + 0.01).ceil() * dx;
        while x < xmax {
            let mut line_width = thickness;
            if (x / self.view.width).abs() < 1e-4 {
                line_width *= 3.0;
            }
            self.draw_line((x, ymin), (x, ymax), line_width, color);
            x += dx;
        }

        let dy = rectify_size(self.view.height / vert_divs as f64);
        let mut y = (ymin / dy + 0.01).ceil() * dy;
        while y < ymax {
            let mut line_width = thickness;
            if (y / self.view.height).abs() < 1e-4 {
                line_width *= 3.0;
            }
            self.draw_line((xmin, y), (xmax, y), line_width, color);
            y += dy;
        }
    }

    pub fn draw_curve<I: Iterator<Item = (f64, f64)>>(
        &mut self,
        mut iter: I,
        thickness: f64,
        color: Color,
    ) {
        let mut last_pt;
        match iter.next() {
            Some(pt) => {
                last_pt = pt;
            }
            None => return,
        }

        for pt in iter {
            self.draw_line(last_pt, pt, thickness, color);
            last_pt = pt;
        }
    }

    // This will be used for a future feature
    #[allow(dead_code)]
    pub fn draw_arrow(
        &mut self,
        base: (f64, f64),
        direc: (f64, f64),
        thickness: f64,
        head_size: f64,
        color: Color,
    ) {
        // Draw body of arrow
        let head = (base.0 + direc.0, base.1 + direc.1);
        self.draw_line(base, head, thickness, color);

        // Rotate direction by 45 degs
        let rt2 = (2.0_f64).sqrt();
        let direc_len = f64::hypot(direc.0, direc.1).max(0.01);
        let right_ear = (
            (direc.0 - direc.1) / (direc_len * rt2),
            (direc.0 + direc.1) / (direc_len * rt2),
        );
        let left_ear = (
            (direc.0 + direc.1) / (direc_len * rt2),
            (-direc.0 + direc.1) / (direc_len * rt2),
        );
        // Use rotated and scaled vectors to draw head
        self.draw_line(
            head,
            (
                head.0 - right_ear.0 * head_size,
                head.1 - right_ear.1 * head_size,
            ),
            thickness,
            color,
        );
        self.draw_line(
            head,
            (
                head.0 - left_ear.0 * head_size,
                head.1 - left_ear.1 * head_size,
            ),
            thickness,
            color,
        );
    }

    // This will be used for a future feature
    #[allow(dead_code)]
    pub fn draw_field<F>(
        &mut self,
        horiz_divs: u8,
        vert_divs: u8,
        arrow_thick: f64,
        color: Color,
        mut field: F,
    ) where
        F: FnMut(f64, f64) -> (f64, f64),
    {
        let (dx, dy) = (
            self.view.width / horiz_divs as f64,
            self.view.height / vert_divs as f64,
        );
        let arrow_scaling = f64::min(dx, dy);
        let arrow_size = arrow_scaling * 0.2;

        let (xmax, ymax) = (
            self.view.xmin + self.view.width,
            self.view.ymin + self.view.height,
        );
        let x_init = (self.view.xmin / dx + 0.01).ceil() * dx;
        let y_init = (self.view.ymin / dy + 0.01).ceil() * dy;

        let mut x = x_init;
        while x < xmax {
            let mut y = y_init;
            while y < ymax {
                let vec = field(x, y);
                self.draw_arrow(
                    (x, y),
                    (arrow_scaling * vec.0, arrow_scaling * vec.1),
                    arrow_thick,
                    arrow_size,
                    color,
                );
                y += dy;
            }
            x += dx;
        }
    }

    pub fn save<P: AsRef<Path>>(&self, path: P) -> Result<(), SaveError> {
        let mut fl = File::create(path).map_err(SaveError::IOError)?;
        self.image
            .write_to(&mut fl, ImageOutputFormat::Png)
            .map_err(SaveError::ImageError)
    }
}
