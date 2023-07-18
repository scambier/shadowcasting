use num_rational::Rational64;

/// This Pos type is expected to be generate enough to allow the user
/// to cast into and out of their own type, such as from the euclid crate.
pub type Pos = (i64, i64);

/// Compute FOV information for a given position using the shadow mapping algorithm.
///
/// This uses the is_blocking closure, which checks whether a given position is
/// blocked (such as by a wall), and is expected to capture some kind of grid
/// or map from the user.
///
/// The mark_visible closure provides the ability to collect visible tiles. This
/// may push them to a vector (captured in the closure's environment), or
/// modify a cloned version of the map.
///
///
/// I tried to write a nicer API which would modify the map as a separate user
/// data, but I can't work out the lifetime annotations.
pub fn compute_fov<F, G>(origin: Pos, is_blocking: &mut F, mark_visible: &mut G)
where
    F: FnMut(Pos) -> bool,
    G: FnMut(Pos, bool),
{
    mark_visible(origin, true);

    for i in 0..4 {
        let quadrant = Quadrant::new(Cardinal::from_index(i), origin);
        let first_row = Row::new(1, Rational64::new(-1, 1), Rational64::new(1, 1));
        scan(first_row, quadrant, is_blocking, mark_visible);
    }
}

fn scan<F, G>(row: Row, quadrant: Quadrant, is_blocking: &mut F, mark_visible: &mut G)
where
    F: FnMut(Pos) -> bool,
    G: FnMut(Pos, bool),
{
    let mut prev_tile = None;

    let mut row = row;

    for tile in row.tiles() {
        let tile_is_wall = is_blocking(quadrant.transform(tile));
        let tile_is_floor = !tile_is_wall;

        let prev_is_wall = prev_tile.map_or(false, |prev| is_blocking(quadrant.transform(prev)));
        let prev_is_floor = prev_tile.map_or(false, |prev| !is_blocking(quadrant.transform(prev)));

        let pos = quadrant.transform(tile);
        if tile_is_wall || is_symmetric(row, tile) {
            mark_visible(pos, true);
        } else {
            mark_visible(pos, false);
        }

        if prev_is_wall && tile_is_floor {
            row.start_slope = slope(tile);
        }

        if prev_is_floor && tile_is_wall {
            let mut next_row = row.next();

            next_row.end_slope = slope(tile);

            scan(next_row, quadrant, is_blocking, mark_visible);
        }

        prev_tile = Some(tile);
    }

    if prev_tile.map_or(false, |tile| !is_blocking(quadrant.transform(tile))) {
        scan(row.next(), quadrant, is_blocking, mark_visible);
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
enum Cardinal {
    North,
    East,
    South,
    West,
}

impl Cardinal {
    fn from_index(index: usize) -> Cardinal {
        use Cardinal::*;
        return [North, East, South, West][index];
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Quadrant {
    cardinal: Cardinal,
    ox: i64,
    oy: i64,
}

impl Quadrant {
    fn new(cardinal: Cardinal, origin: Pos) -> Quadrant {
        return Quadrant {
            cardinal,
            ox: origin.0,
            oy: origin.1,
        };
    }

    fn transform(&self, tile: Pos) -> Pos {
        let (row, col) = tile;

        match self.cardinal {
            Cardinal::North => {
                return (self.ox + col, self.oy - row);
            }

            Cardinal::South => {
                return (self.ox + col, self.oy + row);
            }

            Cardinal::East => {
                return (self.ox + row, self.oy + col);
            }

            Cardinal::West => {
                return (self.ox - row, self.oy + col);
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Row {
    depth: i64,
    start_slope: Rational64,
    end_slope: Rational64,
}

impl Row {
    fn new(depth: i64, start_slope: Rational64, end_slope: Rational64) -> Row {
        return Row {
            depth,
            start_slope,
            end_slope,
        };
    }

    fn tiles(&self) -> impl Iterator<Item = Pos> {
        let depth_times_start = Rational64::new(self.depth, 1) * self.start_slope;
        let depth_times_end = Rational64::new(self.depth, 1) * self.end_slope;

        let min_col = round_ties_up(depth_times_start);

        let max_col = round_ties_down(depth_times_end);

        let depth = self.depth;

        return (min_col..=max_col).map(move |col| (depth, col));
    }

    fn next(&self) -> Row {
        return Row::new(self.depth + 1, self.start_slope, self.end_slope);
    }
}

fn slope(tile: Pos) -> Rational64 {
    let (row_depth, col) = tile;
    return Rational64::new(2 * col - 1, 2 * row_depth);
}

fn is_symmetric(row: Row, tile: Pos) -> bool {
    let (_row_depth, col) = tile;

    let depth_times_start = Rational64::new(row.depth, 1) * row.start_slope;
    let depth_times_end = Rational64::new(row.depth, 1) * row.end_slope;

    let col_rat = Rational64::new(col, 1);

    let symmetric = col_rat >= depth_times_start && col_rat <= depth_times_end;

    return symmetric;
}

fn round_ties_up(n: Rational64) -> i64 {
    return (n + Rational64::new(1, 2)).floor().to_integer();
}

fn round_ties_down(n: Rational64) -> i64 {
    return (n - Rational64::new(1, 2)).ceil().to_integer();
}

#[cfg(test)]
fn inside_map<T>(pos: Pos, map: &Vec<Vec<T>>) -> bool {
    let is_inside = (pos.1 as usize) < map.len() && (pos.0 as usize) < map[0].len();
    return is_inside;
}

#[cfg(test)]
fn matching_visible(expected: Vec<Vec<usize>>, visible: Vec<(i64, i64)>) {
    for y in 0..expected.len() {
        for x in 0..expected[0].len() {
            if visible.contains(&(x as i64, y as i64)) {
                print!("1");
            } else {
                print!("0");
            }
            assert_eq!(expected[y][x] == 1, visible.contains(&(x as i64, y as i64)));
        }
        println!();
    }
}

#[test]
fn test_expansive_walls() {
    let origin = (1, 2);

    let tiles = vec![
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 0, 0, 0, 0, 0, 1],
        vec![1, 0, 0, 0, 0, 0, 1],
        vec![1, 1, 1, 1, 1, 1, 1],
    ];

    let mut is_blocking = |pos: Pos| {
        return !inside_map(pos, &tiles) || tiles[pos.1 as usize][pos.0 as usize] == 1;
    };

    let mut visible = Vec::new();
    let mut mark_visible = |pos: Pos, _active: bool| {
        if inside_map(pos, &tiles) && !visible.contains(&pos) {
            visible.push(pos);
        }
    };

    compute_fov(origin, &mut is_blocking, &mut mark_visible);

    let expected = vec![
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 1, 1, 1, 1, 1, 1],
    ];
    matching_visible(expected, visible);
}

#[test]
fn test_expanding_shadows() {
    let origin = (0, 0);

    let tiles = vec![
        vec![0, 0, 0, 0, 0, 0, 0],
        vec![0, 1, 0, 0, 0, 0, 0],
        vec![0, 0, 0, 0, 0, 0, 0],
        vec![0, 0, 0, 0, 0, 0, 0],
        vec![0, 0, 0, 0, 0, 0, 0],
    ];

    let mut is_blocking = |pos: Pos| {
        return !inside_map(pos, &tiles) || tiles[pos.1 as usize][pos.0 as usize] == 1;
    };

    let mut visible = Vec::new();
    let mut mark_visible = |pos: Pos, _active: bool| {
        if inside_map(pos, &tiles) && !visible.contains(&pos) {
            visible.push(pos);
        }
    };

    compute_fov(origin, &mut is_blocking, &mut mark_visible);

    let expected = vec![
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 1, 0, 0, 1, 1, 1],
        vec![1, 1, 0, 0, 0, 0, 1],
        vec![1, 1, 1, 0, 0, 0, 0],
    ];
    matching_visible(expected, visible);
}

#[test]
fn test_no_blind_corners() {
    let origin = (3, 0);

    let tiles = vec![
        vec![0, 0, 0, 0, 0, 0, 0],
        vec![1, 1, 1, 1, 0, 0, 0],
        vec![0, 0, 0, 1, 0, 0, 0],
        vec![0, 0, 0, 1, 0, 0, 0],
    ];

    let mut is_blocking = |pos: Pos| {
        let outside = (pos.1 as usize) >= tiles.len() || (pos.0 as usize) >= tiles[0].len();
        return outside || tiles[pos.1 as usize][pos.0 as usize] == 1;
    };

    let mut visible = Vec::new();
    let mut mark_visible = |pos: Pos, _active: bool| {
        let outside = (pos.1 as usize) >= tiles.len() || (pos.0 as usize) >= tiles[0].len();

        if !outside && !visible.contains(&pos) {
            visible.push(pos);
        }
    };

    compute_fov(origin, &mut is_blocking, &mut mark_visible);

    let expected = vec![
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![1, 1, 1, 1, 1, 1, 1],
        vec![0, 0, 0, 0, 1, 1, 1],
        vec![0, 0, 0, 0, 0, 1, 1],
    ];
    matching_visible(expected, visible);
}
