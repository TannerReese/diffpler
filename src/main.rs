use clap::{error::ErrorKind, Command};
use std::path::PathBuf;

use diff_eq::{Eqns, State, System};
use plot::{Color, Plot, View};

#[macro_use]
extern crate clap;

#[macro_use]
extern crate lazy_static;

mod diff_eq;
mod parse;
mod plot;

fn parse_coord(s: String) -> Result<(f64, f64), String> {
    let vec = s.as_str().split(',').collect::<Vec<&str>>();
    match TryInto::<[&str; 2]>::try_into(vec) {
        Ok([x_str, y_str]) => match (x_str.parse(), y_str.parse()) {
            (Ok(x), Ok(y)) => Ok((x, y)),
            _ => Err(s),
        },
        Err(_) => Err(s),
    }
}

fn main() {
    let mut prog = Command::new("differ")
    .about("Numerically solve and plot arbitrary order ODEs in multiple variables")
    .args(&[
        arg!(-o --output <IMG_FILE> "File to write the plot to")
            .default_value("plot.png")
            .value_parser(value_parser!(PathBuf)),
        arg!(equations: <EQNS> "")
            .help(concat!(
                "Semicolon separated list of differential equations defining the system\n",
                "(i.e. \"x' = x * z; y''' = sin(x - y); z'' = x * y\")",
            ))
            .value_parser(value_parser!(Eqns)),
        arg!(-i --init <STATE> ... "")
            .required(true)
            .help(concat!(
                "Semicolon separated list of equations defining the initial state\n",
                "(i.e. \"t = -0.5; x = 1.5; y = -3; y' = -0.1; y'' = 5 * cos(pi / 7); z = 4; z' = 3 - pi\")",
            ))
            .value_parser(value_parser!(State)),
        arg!(-x --"horiz-var" [VAR_NAME] "Name of variable to plot on the horizontal axis")
            .default_value("x"),
        arg!(-y --"vert-var" [VAR_NAME] "Name of variable to plot on the vertical axis")
            .default_value("y"),
        arg!(-t --"time-var" [VAR_NAME] "Name of time variable")
            .default_value("t"),
        arg!(-d --duration [TIME] "Maximum duration of time for which to simulate the system")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-L --length [DISTANCE] "Maximum length (per dimension) of the plotted solution")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-s --"time-step" [TIME] "Maximum time elapsed during each step")
            .default_value("0.01")
            .value_parser(value_parser!(f64)),
        arg!(-r --"length-step" [DISTANCE] "Maximum distance (per dimension) to travel during each step")
            .default_value("0.01")
            .value_parser(value_parser!(f64)),
        arg!(-e --center [COORD_PAIR] "Center of viewing window such as \"1.2,-2.1\"")
            .default_value("0.0,0.0"),
        arg!(-W --"win-width" [UNITS] "Width of the viewing window")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-H --"win-height" [UNITS] "Height of the viewing window")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-J --"img-width" [PIXELS] "Width of the image in pixels")
            .default_value("700")
            .value_parser(value_parser!(u32)),
        arg!(-K --"img-height" [PIXELS] "Height of the image in pixels")
            .default_value("700")
            .value_parser(value_parser!(u32)),
        arg!(-c --color [COLOR] ... "Color of solution curves")
            .default_value("red")
            .value_parser(value_parser!(Color)),
        arg!(-B --"bkg-color" [COLOR] "Color of background of the plot")
            .default_value("white")
            .value_parser(value_parser!(Color)),
        arg!(-G --"grid-color" [COLOR] "Color of gridlines")
            .default_value("black")
            .value_parser(value_parser!(Color)),
    ])
    .after_help(concat!(
        "For the initial state, all derivatives less than the one defining a variable must be given.\n",
        "For example, if \"y''' = -x^3\" then y, y', and y'' must be provided in the initial state.\n",
        "If no initial time is provided in the state then 't = 0' will be used.\n",
        "Possible colors are 'white', 'red', 'green', 'blue', 'cyan', 'magenta', 'yellow', and 'black'.\n",
        "Alternatively, one can provide specific colors like 'rgb:134,23,190'\n",
        "\n",
        "Examples:\n",
        "  # Plot a circle\n",
        "  diffpler \"x' = -y; y' = x\" -o circle.png -i \"x=1;y=0\" -d 6.28\n",
        "  # Plot the y value against the t value instead\n",
        "  diffpler \"x' = -y; y' = x\" -o sine.png -i \"t=-pi;x=1;y=0\" -x t\n",
        "  # Plot a higher order system with a different window\n",
        "  diffpler -x u -y v \"u' = cos(v); v'' = sin(u)\" -H 20 -W 20 -i \"u=1;v=2;v'=-0.5\" -o squiggle.png",
    ));
    let mut matches = prog.clone().get_matches();

    let eqns = matches.remove_one("equations").unwrap();
    let time_var: String = matches.remove_one("time-var").unwrap();
    let horiz_var = matches.remove_one("horiz-var").unwrap();
    let vert_var = matches.remove_one("vert-var").unwrap();
    let system = System::new(time_var.clone(), horiz_var, vert_var, eqns)
        .unwrap_or_else(|err| prog.error(ErrorKind::InvalidValue, err).exit());

    // Construct viewing window and plotter
    let center = parse_coord(matches.remove_one("center").unwrap())
        .unwrap_or_else(|err| prog.error(ErrorKind::InvalidValue, err).exit());
    let view = View::new(
        center,
        matches.remove_one("win-width").unwrap(),
        matches.remove_one("win-height").unwrap(),
    );
    let mut plot = Plot::new(
        view,
        matches.remove_one("img-width").unwrap(),
        matches.remove_one("img-height").unwrap(),
        matches.remove_one("bkg-color").unwrap(),
    );

    plot.draw_gridlines(10, 10, 0.5, matches.remove_one("grid-color").unwrap());

    let max_time: f64 = matches.remove_one("duration").unwrap();
    let max_length: f64 = matches.remove_one("length").unwrap();
    let time_step: f64 = matches.remove_one("time-step").unwrap();
    let length_step: f64 = matches.remove_one("length-step").unwrap();
    let state_iter = matches.remove_many("init").unwrap();
    let mut color = *matches.get_one("color").unwrap();
    for mut init in state_iter {
        // Set the initial time to zero if not given
        if !State::has_var(&init, &time_var) {
            init[&time_var] = 0.0;
        }

        // Create the Euler iterator
        let euler_iter = system
            .euler(init, time_step, length_step)
            .unwrap_or_else(|err| prog.error(ErrorKind::InvalidValue, err).exit())
            .take_while(|&(time, length, _)| time < max_time && length < max_length)
            .map(|(_, _, point)| point);

        if let Some(new_color) = matches.remove_one("color") {
            color = new_color;
        }
        plot.draw_curve(euler_iter, 0.5, color);
    }

    let filename: PathBuf = matches.remove_one("output").unwrap();
    if let Err(err) = plot.save(filename) {
        prog.error(ErrorKind::Io, err).exit()
    }
}
