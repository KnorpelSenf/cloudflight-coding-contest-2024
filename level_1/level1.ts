const path = Deno.args[0];
const input = Deno.readTextFileSync(path).split("\r\n").filter((line) =>
    !!line
);

const [count, ...lines] = input;

if (parseInt(count) !== lines.length) throw "bad count";

for (const line of lines) {
    const res = process(line);
    console.log(res);
}

function process(line: string) {
    const [left, right] = line.split(" ");
    const l = parseInt(left);
    const r = parseInt(right);
    const product = l * r;
    const desks = product / 3;
    return desks.toString();
}
