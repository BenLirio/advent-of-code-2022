using DelimitedFiles;
using PaddedViews;

function main()
  n = 99
  data = readdlm("input.txt", ' ', Int, '\n')
  A = Array{Int}(undef, 6, n, n)
  for i in 1:4
      A[i, :, :] = data
  end
  for i in 2:n
    A[1, i, :] = max.(
      A[1, i, :],
      A[1, i-1, :]
    )
    A[2, n+1-i, :] = max.(
      A[2, n+1-i, :],
      A[2, n+1-i+1, :]
    )
    A[3, :, i] = max.(
      A[3, :, i],
      A[3, :, i-1]
    )
    A[4, :, n+1-i] = max.(
      A[4, :, n+1-i],
      A[4, :, n+1-i+1]
    )
  end
  A[5, :, :] = min.(
    A[4, :, :],
    A[3, :, :],
    A[2, :, :],
    A[1, :, :]
  )

  for i in 1:n
    for j in 1:n
      B = PaddedView(-1, A[1:4, :, :], (1:4, 0:(n+1), 0:(n+1)))
      A[6, i, j] = min.(
        B[1, i-1, j],
        B[2, i+1, j],
        B[3, i, j-1],
        B[4, i, j+1]
      )
    end
  end

  return sum(data .> A[6, :, :])
end