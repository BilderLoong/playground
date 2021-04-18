class Solution:
    def convertBST(self, root: TreeNode) -> TreeNode:
        def traverse(node: TreeNode):
            if not node:
                return
            nonlocal total
            traverse(node.right)
            total += node.val
            node.val = total
            traverse(node.left)

        total = 0
        traverse(root)
        return root
