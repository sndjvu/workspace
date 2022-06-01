pub fn slice_split_array<const N: usize, T>(slice: &[T]) -> Option<(&[T; N], &[T])> {
    // Implementation lightly modified from core::slice::split_array_ref.
    if slice.len() < N {
        return None;
    }
    let (a, b) = slice.split_at(N);
    // SAFETY: a points to [T; N]? Yes it's [T] of length N (checked by split_at)
    let array = unsafe { &*a.as_ptr().cast() };
    Some((array, b))
}

pub fn slice_as_arrays<const N: usize, T>(slice: &[T]) -> (&[[T; N]], &[T]) {
    // Implementation copied from core::slice::as_chunks.
    assert_ne!(N, 0);
    let len = slice.len() / N;
    let (multiple_of_n, remainder) = slice.split_at(len * N);
    // SAFETY: We already panicked for zero, and ensured by construction
    // that the length of the subslice is a multiple of N.
    let array_slice = unsafe {
        core::slice::from_raw_parts(
            multiple_of_n.as_ptr().cast(),
            len,
        )
    };
    (array_slice, remainder)
}

pub fn arrays_as_slice<const N: usize, T>(arrays: &[[T; N]]) -> &[T] {
    let len = arrays.len() * N;
    // SAFETY arrays points to the appropriate number of T (arrays don't affect layout)
    unsafe {
        core::slice::from_raw_parts(
            arrays.as_ptr().cast(),
            len,
        )
    }
}
