const path = Deno.args[0];
const input = Deno.readTextFileSync(path)
    .split("\r\n")
    .filter((line) => !!line);

const [count, ...lines] = input;

if (parseInt(count) !== lines.length) throw "bad count";

for (const line of lines) {
    const res = process(line);
    console.log(res);
}

function process(line: string) {
    const [x, y, c] = line.split(" ");
    const rows = parseInt(x);
    // const cols = parseInt(y);
    const count = parseInt(c);
    const res: number[][] = [[]];
    let desk = 1;
    function makeDesk() {
        const d = [desk, desk, desk];
        desk++;
        return d;
    }
    let row = 0;
    function addDesk() {
        const d = makeDesk();
        res[row].push(...d);
        // console.log(rows, res[row])
        if (res[row].length === rows) {
            res.push([]);
            row++;
        }
    }
    for (let i = 0; i < count; i++) {
        addDesk();
    }
    // console.log(res);
    return res.map((row) => row.join(" ")).join("\n");
}
