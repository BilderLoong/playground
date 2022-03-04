export {};
/*
 * function ListNode(x){
 *   this.val = x;
 *   this.next = null;
 * }
 */

/**
 * 代码中的类名、方法名、参数名已经指定，请勿修改，直接返回方法规定的值即可
 *
 * @param k int整型
 * @param a ListNode类 表示数串
 * @return long长整型
 */
interface ListNode {
  val: number;
  next: ListNode;
}
function solve(k: number, a: ListNode) {
  // write code here
  // ((x^y)*k+(x*y))
}
module.exports = {
  solve: solve,
};
