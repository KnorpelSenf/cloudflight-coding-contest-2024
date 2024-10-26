use rayon::prelude::*;
use std::{collections::HashMap, env, fs};

use good_lp::{
    constraint, default_solver, solvers::coin_cbc::CoinCbcProblem, variable, variables, Expression,
    Solution, SolverModel, Variable,
};

fn main() {
    let file_path = env::args().nth(1).expect("Missing path argument!");
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let lines: Vec<(i32, i32, i32)> = contents
        .split("\r\n")
        .skip(1)
        .filter(|line| line.trim().len() > 0)
        .map(|line| {
            line.split(" ")
                .take(3)
                .map(|n| n.parse::<i32>().expect("not a number!"))
                .collect::<Vec<_>>()
        })
        .map(|vec| (vec[0], vec[1], vec[2]))
        .collect();

    let output = lines
        .into_par_iter()
        .map(|(x, y, count)| process(x, y, count).to_string())
        .collect::<Vec<_>>()
        .join("\n\n");
    println!("{output}");
}

struct Room {
    x: i32,
    y: i32,
    positions: Vec<Vec<Position>>,
}
impl ToString for Room {
    fn to_string(&self) -> String {
        let mut iters: Vec<_> = self.positions.iter().map(|n| n.into_iter()).collect();
        (0..self.y)
            .map(|_| {
                iters
                    .iter_mut()
                    .map(|it| *it.next().expect("missing room data"))
                    .collect::<Vec<Position>>()
            })
            .map(|line| {
                line.iter()
                    .map(|pos| pos.to_string())
                    .collect::<Vec<_>>()
                    .join("")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
#[derive(Copy, Clone)]
enum Position {
    X,
    E,
}
impl Into<Position> for f64 {
    fn into(self) -> Position {
        if self < 0.5 {
            Position::E
        } else {
            Position::X
        }
    }
}
impl ToString for Position {
    fn to_string(&self) -> String {
        match self {
            Position::X => String::from("X"),
            Position::E => String::from("."),
        }
    }
}

struct Constraints {
    x: i32,
    y: i32,
    expressions: HashMap<(i32, i32), Expression>,
}
impl Constraints {
    fn new(x: i32, y: i32) -> Self {
        Constraints {
            x,
            y,
            expressions: HashMap::new(),
        }
    }
    fn get_expr(&mut self, x: i32, y: i32) -> Option<&mut Expression> {
        if x < 0 || self.x <= x {
            return None;
        }
        if y < 0 || self.y <= y {
            return None;
        }
        Some(self.expressions.entry((x, y)).or_insert(0.into()))
    }
    fn add_var(&mut self, x: i32, y: i32, var: Variable) {
        for x in x - 1..x + 2 {
            for y in y - 1..y + 2 {
                if let Some(exp) = self.get_expr(x, y) {
                    *exp += var;
                }
            }
        }
    }

    fn add_to(self, model: CoinCbcProblem) -> CoinCbcProblem {
        self.expressions
            .into_values()
            .fold(model, |model, expr| model.with(constraint!(expr <= 4)))
    }
}

fn process(x: i32, y: i32, total: i32) -> Room {
    let mut vars = variables!();
    let mut count: Expression = 0.into();
    let mut constraints = Constraints::new(x, y);
    let ilp_vars = (0..x)
        .into_iter()
        .map(|x| {
            (0..y)
                .map(|y| {
                    let var = vars.add(variable().integer().min(0).max(1));
                    count += var;
                    constraints.add_var(x, y, var);
                    var
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut model = constraints.add_to(vars.maximise(count).using(default_solver));
    model.set_parameter("log", "0");
    let solution = model.solve().expect("no ILP solution");
    let positions: Vec<Vec<Position>> = ilp_vars
        .into_iter()
        .map(|col| col.into_iter().map(|v| solution.value(v).into()).collect())
        .collect();
    let xs: usize = positions
        .iter()
        .map(|col| col.iter().filter(|pos| matches!(pos, Position::X)).count())
        .sum();
    println!("Placed {xs} desks and expected {}", total * 2);
    Room { x, y, positions }
}
