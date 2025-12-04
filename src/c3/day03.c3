// ~/~ begin <<docs/day03.md#docs/day03.md::src/c3/day03.c3>>[init]
module arg_max { Tx };

<*
 @require xs.len > 0 : "array should be non-empty"
 *>
fn usz arg_max(Tx[] xs) {
	usz loc = 0;
	Tx x_max = xs[0];

	foreach (i, x : xs[1..]) {
		if (x > x_max) {
			x_max = x;
			loc = i+1;
		}	
	}

	return loc;
}

module lobby;

import std::io;
import arg_max;

fn usz joltage(int n, char[] bank) {
	usz num = 0;
	usz pos = 0;

	for (int i = n-1; i > 0; i--) {
		usz delta = arg_max { char } (bank[pos..(bank.len-1-i)]);
		num = num * 10 + (long)(bank[pos+delta] - '0');
		pos += delta + 1;
	}

	usz delta = arg_max{char}(bank[pos..]);
	num = num * 10 + (long)(bank[pos + delta] - '0');
	return num;
}

fn void main() => @pool() {
    usz part_1 = 0;
    usz part_2 = 0;
    while (try line = io::readline(tmem)) {
        part_1 += joltage(2, line);
        part_2 += joltage(12, line);
    }
    io::printn("# Day 03");
    io::printfn("Part 1: %d", part_1);
    io::printfn("Part 2: %d", part_2);
}
// ~/~ end
