# %% heapq
import heapq
q = [7, 1, 2, 5, 6]
pq = [(q[i], i) for i in range(len(q))]
print(pq[0])
heapq.heapify(q)
print(q)
# %% compair between pair
print((1, 3), (3, 2))

# will list all the method avaliable on current object
dir(5)

# %% class


class Car:
    def __init__(self):
        print('Initializing the Car class')
        print(self.speed)

    def start(self):
        self.started = True
        print("car started")

    speed = 0
    started = False


class Blue_car(Car):
    def __init__(self):
        print('creating blue car')
    color = 'blue'


car = Car()
blue_car = Blue_car()
blue_car.start()


# %%
print(i)


def foo(i):
    print(i)
