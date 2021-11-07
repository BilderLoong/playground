/*
仓库拣货
详细描述
Shopee 的仓库拣货员接到了一个任务，需要驾驶叉车，在仓库中对一些商品做拣货。

为了便于理解，我们可以将仓库简化为一个二维的棋盘，横向为 x 纵向为 y，左上角的格子坐标为 (0, 0)，跟浏览器视窗的坐标系一样。

商品所在的货架都在不同的格子里，有货架的格子是不可通行的。拣货员只要把叉车开到某个格子的上下左右四个相邻的格子之一，就可以对其进行拣货操作。

拣货员可以给他的叉车下发指令，每次要么向正前方移动一个格子，要么原地转弯（只能左转 90 度、右转 90 度或往后转）。

接到了任务后，拣货员在 (0, 0) 格子坐上叉车，面朝 x 轴正方向（右方），开始了他的工作。由于叉车旋转比较麻烦，他想尽可能减少转弯的次数。

已知整个仓库中所有不可通行的区域，以及任务中每个商品所在的格子位置。可能有很多条路线可以让拣货员拣到所有货物，但请你帮他找到转弯次数最少的那一条。假定拣货完成后，拣货员可以停在最后一个货物的旁边，不需要返回出发点。

你不需要告诉他该如何走，只需要告诉他需要转弯多少次。

其他
时间限制: 1000ms

内存限制: 256.0MB

输入输出描述
输入描述
第一行是两个数字 X、Y，表示仓库的横向为 X 格，纵向为 Y 格；

接下来是一个 X 行 Y 列的字符矩阵，表示仓库的地图，其中：

- "." 表示叉车可以通过；

- "s" 表示这是一个商品，叉车不能通过；

- "x" 表示这是货架的一部分，叉车不能通过。

数据保证每个商品均可以被拣货，不存在某个商品无法到达的情况。

输出描述
一个数字，表示拣完全部的货物所需的最少转弯次数。

备注
对于 30% 的测试用例，仓库的大小为 5*5，任务中只有 1 个商品。

对于全部的测试用例，仓库的大小为 30*30，任务中有 2 个商品。

输入输出示例
示例1
输入
复制
4 3
..x.
....
x..s
输出
复制
2
说明
先往右走到 (1, 0)，顺时针旋转（转完之后朝下），再往下走到 (1, 2)，逆时针旋转（转完之后朝右），走到 (2, 2) 即可拣货。可能的一条路径如下（数字表示走到这里时的转弯次数）：
00x.
.1..
x12s
示例2
输入
复制
4 3
...x
....
x..s
输出
复制
1
说明
先往右走到 (2, 0)，顺时针旋转（转完之后朝下），再往下走到 (2, 2) 即可拣货。可能的一条路径如下（数字表示走到这里时的转弯次数）：
000x
..1.
x.1s
注意：只需要走到相邻的格子即可拣货，拣货之前无需再旋转。
示例3
输入
复制
5 2
.....
..ss.
输出
复制
0
说明
直接往右走，一路边走边拣货，无需转弯。路径如下（数字表示走到这里时的转弯次数）：
0000.
..ss.
*/

export {};
let DEBUG = true;

let s;

if (DEBUG) {
  const Readable = require('stream').Readable;
  s = new Readable();
  s.push('M 10 10 H 90 V 90 H 50 L 10 50 Z');
  s.push(null);
}

const readline = require('readline');
const rl = readline.createInterface({
  input: DEBUG ? s : process.stdin,
  output: process.stdout,
});

const lines: string[] = [];
rl.on('line', (line: string) => {
  let length = 0;

  const arr = line.split(' ');
  let [, x, y] = arr.splice(0, 3).map((e) => parseInt(e));
  const [sx, sy] = [x, y];

  let [dx, dy] = [sx, sy];

  arr.forEach((v, i) => {
    switch (v) {
      case 'L':
        [, dx, dy] = arr.slice(i, i + 3).map((e) => parseInt(e));

        break;

      //Vertical
      case 'V':
        [dy] = arr.slice(i + 1, i + 2).map((e) => parseInt(e));
        break;

      //Horizontal
      case 'H':
        [dx] = arr.slice(i + 1, i + 2).map((e) => parseInt(e));
        break;

      case 'Z':
        [dx, dy] = [sx, sy];

      default:
        break;
    }

    length += Math.sqrt((dx - x) ** 2 + (dy - y) ** 2);
    console.log(length);

    [x, y] = [dx, dy];
  });

  // console.log(length);
});
