from collection import deque

def fill(point, canvas, color):
    if x not in canvas:
        return
    elif y not in canvas[x]:
        return

    x, y = point
    if canvas[y][x] == color:
        return
    canvas[y][x] = color
    fill((x + 1, y), canvas, color)
    fill((x - 1, y), canvas, color)
    fill((x, y + 1), canvas, color)
    fill((x, y - 1), canvas, color)

def fill_bfs(point, canvas, color):
    x, y = point
    if x not in canvas:
        return None
    if y not in canvas[x]:
        return None
    xs = deque()
    xs.append((x, y))
    while xs:
        x, y = xs.popleft()
        canvas[y][x] = color
        for x2, y2 in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
            if x2 not in canvas:
                continue
            elif y2 not in canvas[x2]:
                continue
            if canvas[y2][x2] != color:
                xs.append((x2, y2))
    return None
