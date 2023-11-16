package result

type Result[T any] struct {
	value T
	err   error
}

// By value rather than by pointer.
func (r Result[T]) Unwrap() T {
	if r.err != nil {
		panic(r.err.Error())
	}

	return r.value
}

func New[T any](v T, err error) Result[T] {
	return Result[T]{v, err}
}

func Ok[T any](v T) Result[T] {
	return Result[T]{value: v, err: nil}
}

func (r *Result[T]) GetOK() T {
	if r.err != nil {
		panic(r.err.Error())
	}

	return r.value
}

func (r *Result[T]) GetError() error {
	if r.err == nil {
		panic("This result has no error")
	}

	return r.err
}

func Bind[T any, R any](a Result[T], f func(T) (R, error)) Result[R] {
	if a.err != nil {
		return Result[R]{err: a.err}
	}

	v, err := f(a.value)
	if err != nil {
		return Result[R]{err: err}
	}

	return Result[R]{value: v, err: nil}
}
