import math

def calculate_prize(distance):
    base_rate = 2.5
    total_prize = 0
    distance = math.floor(distance - 5)

    if distance < 1:
        return 0

    while distance >= 0:
        if distance >= 5:
            extra_km = 1
            # print(total_prize)
            total_prize += (5 + extra_km) * base_rate
            distance -= 5
            base_rate += 0.5
        else:
            total_prize += distance * base_rate
            return total_prize


# Test the function
# print(calculate_prize(50))
print(calculate_prize(10000* 10000))

assert calculate_prize(10) == 6 * 2.5
assert calculate_prize(11) == 6 * 2.5 + 1 * 3
assert calculate_prize(12) == 6 * 2.5 + 2 * 3
assert calculate_prize(15) == 6 * 2.5 + 6 * 3
assert calculate_prize(16) == 6 * 2.5 + 6 * 3 + 1 * 3.5
assert calculate_prize(16.1) == 6 * 2.5 + 6 * 3 + 1 * 3.5

