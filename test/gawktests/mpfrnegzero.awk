BEGIN {
	printf("-0 -> %f, -0.0 -> %f\n", -0, -0.0)

	printf("atan2(+0, -0) = %f\n", atan2(+0, -0))
	printf("atan2(+0.0, -0.0) = %f\n", atan2(+0.0, -0.0))

	printf("atan2(-0, -0) = %f\n", atan2(-0, -0))
	printf("atan2(-0.0, -0.0) = %f\n", atan2(-0.0, -0.0))

	printf("atan2(+0, +0) = %f\n", atan2(+0, +0))
	printf("atan2(+0.0, +0.0) = %f\n", atan2(+0.0, +0.0))

	printf("atan2(-0, +0) = %f\n", atan2(-0, +0))
	printf("atan2(-0.0, +0.0) = %f\n", atan2(-0.0, +0.0))
}
