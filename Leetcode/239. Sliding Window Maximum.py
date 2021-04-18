import heapq


class Solution:
    # using a prior queue to store the max value
    def maxSlidingWindow(self, nums: list[int], k: int) -> list[int]:
        q = [(-nums[i], i) for i in range(k)]
        heapq.heapify(q)

        res = [-q[0][0]]

        for i in range(k, len(nums)):
            heapq.heappush(q, (-nums[i], i))
            while q[0][1] <= i-k:
                heapq.heappop(q)
            res.append(-q[0][0])

        return res


class Solution:
    def maxSlidingWindow(self, nums: list[int], k: int) -> list[int]:
        q = collections.deque()
        res = []

        for i in range(k):
            while q and nums[i] > nums[q[-1]]:
                q.pop()
            q.append(i)

        res.append(nums[q[0]])

        for i in range(k, len(nums)):
            while q and nums[i] > nums[q[-1]]:
                q.pop()
            q.append(i)
            while q[0] <= i - k:
                q.popleft()

            res.append(nums[q[0]])

        return res


input = [1, 3, -1, -3, 5, 3, 6, 7]

res = Solution().maxSlidingWindow(input, 3)
print(res)
