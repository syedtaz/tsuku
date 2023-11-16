package list

func Iter[T any](a []T, f func(T)) {
	for _, v := range a {
		f(v)
	}
}

func Map[T any, R any](a []T, f func(T) R) []R {
	results := make([]R, len(a))
	for _, v := range a {
		results = append(results, f(v))
	}
	return results
}

func Filter[T any](a []T, f func(T) bool) []T {
	results := make([]T, 0, len(a))

	for _, v := range a {
		if f(v) {
			results = append(results, v)
		}
	}

	return results
}
