local loop1 = coroutine.create(function()
    while true do
        print('loop1')
        coroutine.yield()
    end
end)


local loop2 = coroutine.create(function()
    while true do
        print('loop2')
        coroutine.yield()
    end
end)

while true do
    coroutine.resume(loop1)
    coroutine.resume(loop2)
end
