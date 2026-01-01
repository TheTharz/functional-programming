type Position = (Int, Int)

possibleMoves :: [Position]
possibleMoves = [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2)]

moves :: Position -> [Position]
moves (x,y) = [(x+dx,y+dy) | (dx,dy)<-possibleMoves, onBoard (x+dx,y+dy)] 

onBoard :: Position -> Bool
onBoard (x,y) = x <= 8 && x>=1 && y<=8 && y>=1