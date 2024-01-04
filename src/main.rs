use std::path::PathBuf;
use clap::{Command, error::ErrorKind};

use diff_eq::{Eqns, System, State};
use plot::{Plot, View};

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
            .value_parser(value_parser!(PathBuf)),
        arg!(equations: <EQNS> "List of equations defining the system")
            .long_help(concat!(
                "Semicolon separated list of differential equations defining the system\n",
                "(i.e. \"x' = x * z; y''' = sin(x - y); z'' = x * y\")i",
            ))
            .value_parser(value_parser!(Eqns)),
        arg!(-i --init <STATE> ... "List of initial values for the variables")
            .long_help(concat!(
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
        arg!(-d --duration [TIME] "Duration of time variable for which to simulate the system")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-s --step [TIME] "Amount of time elapsed for each step of numerical approximation")
            .default_value("0.01")
            .value_parser(value_parser!(f64)),
        arg!(-c --center [COORD_PAIR] "Center of viewing window such as \"1.2,-2.1\"")
            .default_value("0.0,0.0"),
        arg!(-W --"win-width" [UNITS] "Width of the viewing window")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-H --"win-height" [UNITS] "Height of the viewing window")
            .default_value("10.0")
            .value_parser(value_parser!(f64)),
        arg!(-C --"img-width" [PIXELS] "Width of the image in pixels")
            .default_value("700")
            .value_parser(value_parser!(u32)),
        arg!(-R --"img-height" [PIXELS] "Height of the image in pixels")
            .default_value("700")
            .value_parser(value_parser!(u32)),
    ])
    .after_help(concat!(
        "For the initial state, all derivatives less than the one defining a variable must be given.\n",
        "For example, if \"y''' = -x^3\" then y, y', and y'' must be provided in the initial state.\n",
        "If no initial time is provided in the state then 't = 0' will be used.\n",
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
    let time_var = matches.remove_one("time-var").unwrap();
    let system = System::new(time_var, eqns).unwrap_or_else(|err| prog.error(ErrorKind::InvalidValue, err).exit());
    
    let center = parse_coord(matches.remove_one("center").unwrap())
        .unwrap_or_else(|err| prog.error(ErrorKind::InvalidValue, err).exit());
    let view = View::new(center,
        matches.remove_one("win-width").unwrap(),
        matches.remove_one("win-height").unwrap(),
    );
    let mut plot = Plot::new(view,
        matches.remove_one("img-width").unwrap(),
        matches.remove_one("img-height").unwrap(),
        plot::COLOR_BLACK,
    );

    plot.draw_gridlines(10, 10, 0.5, plot::COLOR_WHITE);

    let duration: f64 = matches.remove_one("duration").unwrap();
    let step_size: f64 = matches.remove_one("step").unwrap();
    let step_count = (duration / step_size).ceil() as usize;
    let horiz_var: String = matches.remove_one("horiz-var").unwrap();
    let vert_var: String = matches.remove_one("vert-var").unwrap();
    let state_iter = matches.remove_many("init").unwrap();
    for init in state_iter {
        let euler_iter = system.euler(step_size, init)
            .unwrap_or_else(|err| prog.error(ErrorKind::InvalidValue, err).exit())
            .take(step_count)
            .map(|state| (state[&horiz_var], state[&vert_var]));
        plot.draw_curve(euler_iter, 0.5, plot::COLOR_RED);
    }

    let filename: PathBuf = matches.remove_one("output").unwrap();
    match plot.save(filename) {
        Err(err) => prog.error(ErrorKind::Io, err).exit(),
        Ok(_) => return,
    }
}
