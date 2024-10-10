function add_flags(old) {
  if (old)
    return 0
  if (!old)
    return 1
}
BEGIN {
  a[0]=add_flags(a[0])
}
