pathInBox =
function(path, box)
{
  b = apply(path, 2, range)

  all(box[1, 1] <= b[1 , 1], box[2, 1] >= b[2, 1],
      box[1, 2] <= b[1, 2], box[2, 2] >= b[2,2])
}
