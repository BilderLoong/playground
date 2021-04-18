# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
from collections import deque


# iterate
class Solution:
    def isSymmetric(self, root: TreeNode) -> bool:
        return self.check(root, root)

    def check(self, p: TreeNode, q: TreeNode) -> bool:
        que = deque()
        que.extend([p, q])

        while que:
            l = que.popleft()
            r = que.popleft()

            if l is None and r is None:
                continue
            if ((l is None or r is None) or l.val != r.val):
                return False
            que.append(l.left)
            que.append(r.right)

            que.append(l.right)
            que.append(r.left)

        return True
