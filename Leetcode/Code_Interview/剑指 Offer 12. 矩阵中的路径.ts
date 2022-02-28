export default {};

function exist(board: string[][], word: string): boolean {
  for (let i = 0; i < board.length; i++) {
    const row = board[i];

    for (let j = 0; j < row.length; j++) {
      if (
        traverse(
          board,
          i,
          j,
          word,
          0,
          matrixMaker(board.length, board[0].length, false)
        )
      ) {
        return true;
      }
    }
  }

  return false;

  function traverse(
    board: string[][],
    row: number,
    col: number,
    word: string,
    index: number,
    boolBoard: boolean[][]
  ): boolean {
    if (
      !(row >= 0 && row < board.length) ||
      !(col >= 0 && col < board[0].length) ||
      board[row][col] !== word[index] || // not the target char
      boolBoard[row][col] // already visited
    )
      return false;

    if (index === word.length - 1) return true;

    boolBoard[row][col] = true;

    const left = traverse(board, row, col - 1, word, index + 1, boolBoard);
    const right = traverse(board, row, col + 1, word, index + 1, boolBoard);
    const top = traverse(board, row - 1, col, word, index + 1, boolBoard);
    const below = traverse(board, row + 1, col, word, index + 1, boolBoard);
    const res = left || right || top || below;

    if (!res) boolBoard[row][col] = false; //

    return res;
  }
}

function matrixMaker<T>(row: number, col: number, val: T): T[][] {
  const matrix = [];
  for (let i = 0; i < row; i++) {
    const arr = new Array(col);
    arr.fill(val);
    matrix.push(arr);
  }

  return matrix;
}
