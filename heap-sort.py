def max_heapify(arr, root, n):
    largest = root
    left = root * 2 + 1
    right = left + 1

    if left < n and arr[left] > arr[largest]:
        largest = left

    if right < n and arr[right] > arr[largest]:
        largest = right

    if largest != root:
        arr[root], arr[largest] = arr[largest], arr[root]
        # now the root is at the postion larget
        max_heapify(arr, largest, n)


def max_heap_builder(arr):
    for i in range(len(arr)//2 - 1, -1, -1):
        max_heapify(arr, i, len(arr))


def heap_sort(arr):
    for i in range(len(arr)-1, 0, -1):
        arr[0], arr[i] = arr[i], arr[0]
        max_heapify(arr, 0, i)


# Driver code
arr = [12, 11, 13, 5, 6, 7]
max_heap_builder(arr)
print("heap:", arr)
heap_sort(arr)
print("result:", arr)
# %%
print([i for i in range(2,10)])
