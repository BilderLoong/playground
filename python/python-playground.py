map_1 = {"a": 1}


def foo(param: dict[str, int]):
    param["b"] = 1
    print(param)
    # print(param)


foo(dict(map_1))
print(map_1)
