class Solution:
    def nextGreaterElements(self, nums):
        # store the index in the stack
        n = len(nums)
        res = [-1] * n
        stack = list()

        for i in range(n * 2-1):
            while stack and nums[i % n] > nums[stack[-1]]:
                res[stack.pop()] = nums[i % n]
            stack.append(i % n)
        return res


print(Solution().nextGreaterElements([1, 2, 1]))
