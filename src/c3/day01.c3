// ~/~ begin <<docs/day01.md#day01::src/c3/day01.c3>>[init]
module day01;
import std::io;
import std::math;

// ~/~ begin <<docs/day01.md#day01::c3-dial>>[init]
struct Dial
{
    int position;
    int n_stops_zero;
    int n_passes_zero;
}

fn int modulo(int a, int b) @inline {
    int c = a % b;
    return c < 0 ? b + c : c;
}

fn void update_dial(Dial *dial, int instr) {
    int newpos = dial.position + instr;

    dial.n_passes_zero += math::abs(newpos / 100);
    if (newpos < 0 && dial.position != 0 || newpos == 0) {
        dial.n_passes_zero += 1;
    }

    dial.position = modulo(newpos, 100);

    if (dial.position == 0) {
        dial.n_stops_zero += 1;
    }
}
// ~/~ end
// ~/~ begin <<docs/day01.md#day01::c3-reading>>[init]
fn int? read_instruction() => @pool() {
    String line = io::readline(tmem)!;
    if (line[0] == 'L') {
        return -line[1..].to_int();
    } else {
        return line[1..].to_int();
    }
}
// ~/~ end
// ~/~ begin <<docs/day01.md#day01::c3-main>>[init]
fn void main()
{
    io::printn("# Day 01");
    Dial dial = {50, 0, 0};
    while (try instr = read_instruction()) {
        update_dial(&dial, instr);
    }
    io::printfn("Part 1: %d\nPart 2: %d", dial.n_stops_zero, dial.n_passes_zero);
}
// ~/~ end
// ~/~ end
