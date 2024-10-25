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
    console.log("------", line);
    const [x, y, c] = line.split(" ");
    const rows = parseInt(x);
    const cols = parseInt(y);
    const count = parseInt(c);

    const room: number[][] = Array(rows).fill(0).map(() => Array(cols).fill(0));
    console.log(room);

    let desk = 1;
    function placeDeskHor(row: number, col: number) {
        if (desk > count) return;
        room[row][col] = desk;
        room[row][col + 1] = desk;
        room[row][col + 2] = desk;
        desk++;
    }
    function placeDeskVert(row: number, col: number) {
        if (desk > count) return;
        room[row][col] = desk;
        room[row + 1][col] = desk;
        room[row + 2][col] = desk;
        desk++;
    }

    const desksPerRow = Math.floor(cols / 3);
    for (let i = 0; i < rows; i++) {
        for (let j = 0; j < desksPerRow; j += 3) {
            placeDeskHor(i, j);
        }
    }
    const space = cols % 3;
    const desksPerCol = Math.floor(rows / 3);
    for (let i = cols - space; i < cols; i++) {
        for (let j = 0; j < desksPerCol; j += 3) {
            placeDeskVert(j, i);
        }
    }

    return room.map((row) => row.join(" ")).join("\n");
}
