transposeMatrix = function(mat)
{
	t(mat);	
}

rotateMatrix90 = function(mat)
{
	rotationMatrix = matrix ( c (
	0, 0, 1,
	0, 1, 0,
	1, 0, 0
	), nrow=3, byrow=T);
	
	result <- t(mat) %*% rotationMatrix 
	result;
}

rotateMatrix180 = function(mat)
{
	result <- rotateMatrix90(rotateMatrix90(mat))
	result;
}

rotateMatrix270 = function(mat)
{
	rotationMatrix = matrix ( c (
	0, 0, 1,
	0, 1, 0,
	1, 0, 0
	), nrow=3, byrow=T);
	
	result <- rotationMatrix %*% t(mat)
	result;
}