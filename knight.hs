type Position = (Int, Int)

possibleMoves :: [Position]
possibleMoves = [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2)]

moves :: Position -> [Position]
moves (x,y) = [(x+dx,y+dy) | (dx,dy)<-possibleMoves, onBoard (x+dx,y+dy)] 

onBoard :: Position -> Bool
onBoard (x,y) =and [x <= 8, y <= 8, x > 0, y > 0]