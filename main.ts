const file = Deno.args[0];
const [count, ...input] = Deno
    .readTextFileSync(file)
    .split("\r\n")
    .filter((line) => !!line);
const n = parseInt(count);

type Field = "." | "X";
type Move = "W" | "A" | "S" | "D";

for (let i = 0; i < n; i++) {
    // console.log("Task", i);
    const [, hStr] = input.shift()!.split(" ");
    const h = parseInt(hStr);
    const rows: Field[][] = [];
    for (let r = 0; r < h; r++) {
        const row = input.shift()!;
        const fields = [...row] as Field[];
        rows.push(fields);
    }
    const path = [...input.shift()!] as Move[];
    const possible = check(rows, path);
    console.log(possible ? "VALID" : "INVALID");
}

function findDim(path: Move[]) {
    let left = 0, right = 1, top = 0, bottom = 1;
    let row = 0, col = 0;
    for (const m of path) {
        switch (m) {
            case "W":
                row--;
                break;
            case "A":
                col--;
                break;
            case "S":
                row++;
                break;
            case "D":
                col++;
                break;
        }
        if (row < top) top = row;
        else if (row >= bottom) bottom = row + 1;

        if (col < left) left = col;
        else if (col >= right) right = col + 1;
    }

    return {
        width: right - left,
        height: bottom - top,
        startRow: 0 - top,
        startCol: 0 - left,
        endRow: row - top,
        endCol: col - left,
    };
}

function check(field: Field[][], path: Move[]): boolean {
    const dim = findDim(path);
    // console.log(dim);
    // for (const row of field) {
    // console.log(...row);
    // }
    // console.log(path.join(""));

    console.log(
        "-----------------",
        field.length,
        "rows",
        field[0].length,
        "cols",
    );
    console.log(dim);
    for (const row of field) {
        console.log(...row);
    }
    console.log(path.join(""));
    if (dim.height !== field.length) {
        console.log("row mismatch!");
        return false;
    }
    if (dim.width !== field[0].length) {
        console.log("col mismatch!");
        return false;
    }

    let row = dim.startRow;
    let col = dim.startCol;
    field[row][col] = "X";
    for (const m of path) {
        for (const row of field) {
            console.log(...row);
        }
        console.log(m);
        // console.log("go from", x, y);
        switch (m) {
            case "W":
                row--;
                break;
            case "A":
                col--;
                break;
            case "S":
                row++;
                break;
            case "D":
                col++;
                break;
        }
        // console.log("to", x, y);
        if (field[row][col] !== ".") {
            console.log("hit", field[row][col], "at", row, col);
            return false;
        }
        field[row][col] = "X";
    }

    console.log("Path fits, checking exhaustiveness");
    for (const row of field) {
        console.log(...row);
    }
    return field.every((row) => row.every((f) => f === "X"));
}
