use std::io::stdin;
use std::collections::{HashSet};
use std::mem::{replace, swap};

type Unit = i8;
type Point = (Unit, Unit);

fn main() {
    // user control of the program
    let cycles: usize = std::env::var("TARGET")
        .map(|cycles| cycles.parse().unwrap())
        .unwrap_or(1_000_000_000);
    let interval: usize = std::env::var("INTERVAL")
        .map(|interval| interval.parse().unwrap())
        .unwrap_or((cycles / 1000).max(1));
    let should_print: bool = std::env::var("PRINT")
        .map(|var| var == "1")
        .unwrap_or(false);
    
    // parsing the input
    let mut lines: Vec<String> = stdin()
        .lines()
        .map(|r| r.ok())
        .flatten()
        .collect();
    for line in lines.iter_mut() { *line = format!("#{line}#") }
    let width  = lines[0].len() as Unit;
    lines.insert(0, "#".to_string().repeat(width as usize));
    lines.push("#".to_string().repeat(width as usize));
    let height = lines.len() as Unit;
    
    let mut rocks = HashSet::new();
    let mut blocks = HashSet::new();
    let mut blocks_as_vec = Vec::new();

    let (mut a, mut b) = (vec![0; width as usize * height as usize as usize], vec![0; width as usize * height as usize]);
    let (mut front, mut back) = (&mut a[..], &mut b[..]);

    let index = |row: Unit, column: Unit| row as usize * width as usize + column as usize as usize;

    let north : (Unit, Unit) = (-1,  0);
    let south : (Unit, Unit) = ( 1,  0);
    let east  : (Unit, Unit) = ( 0,  1);
    let west  : (Unit, Unit) = ( 0, -1);

    for (row, line) in lines.iter().enumerate() {
        for (column, c) in line.chars().enumerate() {
            if c == '#' {
                blocks.insert((row as Unit, column as Unit));
                blocks_as_vec.push((row as Unit, column as Unit));
            } else if c == 'O' {
                rocks.insert((row as Unit, column as Unit));
            }
        }
    }
    
    // magnet LUT construction
    let magnetise = |(dr, dc): Point| {
        let mut magnet = vec![(-1, -1); width as usize * height as usize];
        for &(blockrow, blockcolumn) in &blocks_as_vec {
            let (mut row, mut column) = (blockrow - dr, blockcolumn - dc);
            while 0 <= row && row < height && 0 <= column && column < width && !blocks.contains(&(row, column)) {
                magnet[index(row, column)] = (blockrow, blockcolumn);
                row -= dr;
                column -= dc;
            }
        }
        magnet
    };
    let north_magnet = magnetise(north);
    let west_magnet = magnetise(west);
    let south_magnet = magnetise(south);
    let east_magnet = magnetise(east);
    println!("Constructed block LUT");
    
    // formatting code here
    let print = |front: &[Unit], magnet: &[Point]| {
        for row in 0..height {
            for column in 0..width {
                if blocks.contains(&(row, column)) {
                    print!("#");
                } else {
                    let (blockrow, blockcolumn) = magnet[index(row, column)];
                    let distance = (blockrow - row) + (blockcolumn - column);
                    if distance.abs() <= front[index(blockrow, blockcolumn)] {
                        print!("O");
                    } else {
                        print!(".");
                    }
                }
            }
            println!();
        }
    };

    // engage function
    let engage = |front: &mut [Unit], back: &mut [Unit], (pdr, pdc): Point, magnet: &[Point]| {
        for block in 0..blocks_as_vec.len() {
            let (blockrow, blockcolumn) = blocks_as_vec[block];
            for d in 1..1+replace(&mut front[index(blockrow, blockcolumn)], 0) {
                let (rockrow, rockcolum) = (blockrow - pdr * d, blockcolumn - pdc * d);
                // println!("Rock is {rock:?} and block is {block:?} and dir is ({pdr}, {pdc}) ({count})");
                let (blockrow, blockcolumn) = magnet[index(rockrow, rockcolum)];
                back[index(blockrow, blockcolumn)] += 1;
            }
        }
    };
    
    // score function
    let score = |front: &[Unit], (dr, _): Point| blocks_as_vec.iter()
        .fold(0, |mut score, &(blockrow, blockcolumn)| {
            let count = front[index(blockrow, blockcolumn)];
            for d in 1..count+1 {
                let rockrow = blockrow - dr * d;
                score += (height - 2) - (rockrow - 1);
            }
            score
        });

    // bootstrap cycle
    for (rockrow, rockcolum) in rocks {
        let (blockrow, blockcolumn) = north_magnet[index(rockrow, rockcolum)];
        back[index(blockrow, blockcolumn)] += 1;
    }
    swap(&mut front, &mut back);
    engage(front, back, north, &west_magnet);
    swap(&mut front, &mut back);
    engage(front, back, west, &south_magnet);
    swap(&mut front, &mut back);
    engage(front, back, south, &east_magnet);
    swap(&mut front, &mut back);

    println!("Completed first move north");
    
    // cycle loop
    for i in 2..cycles+1 {
        engage(front, back, east, &north_magnet);
        swap(&mut front, &mut back);
        engage(front, back, north, &west_magnet);
        swap(&mut front, &mut back);
        engage(front, back, west, &south_magnet);
        swap(&mut front, &mut back);
        engage(front, back, south, &east_magnet);
        swap(&mut front, &mut back);
        if i % interval == 0 {
            println!("Score is: {} at {i}", score(&front, east));
            if should_print { print(&front, &east_magnet) };
        }
    }

    println!("Score is: {} at {cycles}", score(&front, east));
    if should_print { print(&front, &east_magnet) };

}
