import "lib/github.com/diku-dk/segmented/segmented"
let newline = u8.i32 '\n'

let ascii_digit_to_i32 (digit: u8): i32 =
	i32.u8 (digit - (u8.i32 '0'))

let split_lines [n] (file: [n]u8): ([]u8, []bool, []i32, []i32) = 
	let file =  (concat_to (n+1) "\n" file) in
	let flags = map (newline ==) file in
	let lengths = segmented_reduce (+) (0) flags (replicate (n+1) 1i32) in
	let offsets = scan (+) (0) lengths in
	(file, flags, lengths, offsets)

let correct_endian_worth_map [n] [m] (flags: [n]bool) (lengths: [m]i32): [n]i32 =
	map (\x -> (lengths[x.1] - (i32.i64 x.0))) (
		let position_worth_wrong_endian: [n]i64 = segmented_iota flags in
		let group_index = replicated_iota (map i32.to_i64 lengths) in
		zip (position_worth_wrong_endian) (group_index :> [n]i64)
		)

let sum_within_groups [n] (values: [n]i32) (lengths: []i32): []i32 =
	let gap_map = map (1 ==) (lengths :> [n]i32) in
	segmented_reduce (+) (0) gap_map values

let  part1__ (file: []u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let endian_worthness_map = correct_endian_worth_map flags lengths in
	let worthyness_adjusted_digit_values = map2 (\x y -> if (x == newline) then 0 else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map in 
	let digit_string_values = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values in
	let group_sums = sum_within_groups digit_string_values lengths in
	let max_sum = i32.maximum group_sums in
		(file, flags, lengths, offsets, endian_worthness_map, worthyness_adjusted_digit_values, digit_string_values, group_sums, max_sum)


let max3 [n] (values: [n]i32): (i32, i32, i32) =
	let neutral = (i32.lowest, i32.lowest, i32.lowest) in
	let merge (a, b, c) (d, e, f) = (
		if a > d then -- a max1; keep
			if b > d then -- b max2; keep
				if c > d then -- c max3; keep
					(a, b, c)
				else -- d max3; keep
					(a, b, d)
			else -- d max2; keep
				if b > e then -- b max3;  keep
					(a, d, b)
				else -- e max3; keep
					(a, d, e)
		else -- d max1; keep
			if e > a then -- e max2; keep
				if f > a then -- f max3; keep
					(d, e, f)
				else -- a max3; keep
					(d, e, a)
			else -- a max2; keep
				if e > b then -- e max3;  keep
					(d, a, e)
				else -- b max3; keep
					(d, a, b)
		) in
	reduce_comm (merge) (neutral) (map (\x -> (x, i32.lowest, i32.lowest)) values)

let part1___ [n] (file: [n]u8) =
	let m = n/4 in
	let unflattened = unflatten m 4 file in
	let (A, B, C, X, Y, Z) = (u8.i32 'A', u8.i32 'B', u8.i32 'C', u8.i32 'X', u8.i32 'Y', u8.i32 'Z') in
	let (R, P, S, Draw, Winn, Loss) = (1, 2, 3, 3, 6, 0) in
	-- X/A : Rock, Y/B : Paper, Z/C: Scissors
	-- Rock > Scissors, Scissors > Paper, Paper > Rock
	let map_mine they me =
		if (me == X && they == A) then (R + Draw) else
		if (me == X && they == B) then (R + Loss) else
		if (me == X && they == C) then (R + Winn) else
		if (me == Y && they == A) then (P + Winn) else
		if (me == Y && they == B) then (P + Draw) else
		if (me == Y && they == C) then (P + Loss) else
		if (me == Z && they == A) then (S + Loss) else
		if (me == Z && they == B) then (S + Winn) else
		if (me == Z && they == C) then (S + Draw) else
		0 in
	let round_scores = map (\(x: [4]u8) -> map_mine x[0] x[2]) unflattened in
	let round_picks = map (\(x: [4]u8) -> ( x[0], x[2])) unflattened in
	(unflattened, round_scores, round_picks)


let part2___ [n] (file: [n]u8) =
	let m = n/4 in
	let unflattened = unflatten m 4 file in
	let (A, B, C, X, Y, Z) = (u8.i32 'A', u8.i32 'B', u8.i32 'C', u8.i32 'X', u8.i32 'Y', u8.i32 'Z') in
	let (R, P, S, Draw, Winn, Loss) = (1, 2, 3, 3, 6, 0) in
	-- A : Rock, B : Paper, C: Scissors
	-- X : Loss, Y : Draw, Z : Win
	-- Rock > Scissors, Scissors > Paper, Paper > Rock
	let map_mine they me =
		if (me == X && they == A) then (S + Loss) else
		if (me == X && they == B) then (R + Loss) else
		if (me == X && they == C) then (P + Loss) else
		if (me == Y && they == A) then (R + Draw) else
		if (me == Y && they == B) then (P + Draw) else
		if (me == Y && they == C) then (S + Draw) else
		if (me == Z && they == A) then (P + Winn) else
		if (me == Z && they == B) then (S + Winn) else
		if (me == Z && they == C) then (R + Winn) else
		0 in
	let round_scores = map (\(x: [4]u8) -> map_mine x[0] x[2]) unflattened in
	let round_picks = map (\(x: [4]u8) -> ( x[0], x[2])) unflattened in
	(unflattened, round_scores, round_picks)



let first_halves_map [n] [m] (flags: [n]bool) (lengths: [m]i32): [n]bool =
	map (\x -> (lengths[x.1] > (2 * ((i32.i64 x.0)-0)))) (
		let position_worth_wrong_endian: [n]i64 = segmented_iota flags in
		let group_index = replicated_iota (map i32.to_i64 lengths) in
		zip (position_worth_wrong_endian) (group_index :> [n]i64)
		)

let part1_ [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let letter_to_bit_masks (letter: u8): (u32, u32) = (
		let case_mask = 32u8 in
		let uppercase_is_zero = case_mask & letter in
		let shift = letter & 31u8 in
		let lower_shift = if uppercase_is_zero == 0 then 0 else shift in
		let upper_shift = if uppercase_is_zero == case_mask then 0 else shift in
		(1u32 << (u32.u8 lower_shift), 1u32 << (u32.u8 upper_shift))
	) in
	let mapped_letters = map (\x -> if x == newline then (0, 0) else letter_to_bit_masks x) file in
	let first_halves = first_halves_map flags lengths in 
	let zipper_letters = zip first_halves mapped_letters in
	let reduced_letters = segmented_reduce (\x y -> ((x.0.0 | y.0.0, x.0.1 | y.0.1), (x.1.0 | y.1.0, x.1.1 | y.1.1)) ) ((1u32, 1u32), (1u32, 1u32)) (flags) (map (\x -> if x.0 then (x.1, (1u32, 1u32)) else ((1u32, 1u32), x.1) ) zipper_letters) -- (thing that splits letter mappings into a first half and a second half branch and keeps them separate until return from this segmented reduce; comparing first with second halves happens AFTER this reduce)
	in let crushed_letters = map (\x -> (x.0.0 & x.1.0, x.0.1 & x.1.1)) reduced_letters in
	let value_rough_letters = map (\x -> ((u32.ctz (x.0 ^ 1u32) |> u32.i32) & !32u32, (u32.ctz (x.1 ^ 1u32) |> u32.i32) & !32u32 )) crushed_letters in
	let priority_values = map (\x -> if x.0 != 0 then x.0 else x.1 + 26) value_rough_letters in
	let priority_sum = u32.sum (init priority_values) in

	(flags, lengths, offsets, first_halves, reduced_letters, crushed_letters, value_rough_letters, priority_values, priority_sum)


let letter_grouper [n] (letters: [n](u32, u32)) =
	let m = n/3 in
	unflatten m 3 letters

let part2_ [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let letter_to_bit_masks (letter: u8): (u32, u32) = (
		let case_mask = 32u8 in
		let uppercase_is_zero = case_mask & letter in
		let shift = letter & 31u8 in
		let lower_shift = if uppercase_is_zero == 0 then 0 else shift in
		let upper_shift = if uppercase_is_zero == case_mask then 0 else shift in
		(1u32 << (u32.u8 lower_shift), 1u32 << (u32.u8 upper_shift))
	) in
	let mapped_letters = map (\x -> if x == newline then (0, 0) else letter_to_bit_masks x) file in
	--let first_halves = first_halves_map flags lengths in 
	--let zipper_letters = zip first_halves mapped_letters in
	let reduced_letters = segmented_reduce (\x y -> ((x.0 | y.0, x.1 | y.1)) ) (1u32, 1u32) (flags) (mapped_letters) -- (thing that splits letter mappings into a first half and a second half branch and keeps them separate until return from this segmented reduce; comparing first with second halves happens AFTER this reduce)
	in let grouped_letters = letter_grouper (init reduced_letters)
	let crushed_letters = map (\x -> (x[0].0 & x[1].0 & x[2].0, x[0].1 & x[1].1 & x[2].1)) grouped_letters in
	let value_rough_letters = map (\x -> ((u32.ctz (x.0 ^ 1u32) |> u32.i32) & !32u32, (u32.ctz (x.1 ^ 1u32) |> u32.i32) & !32u32 )) crushed_letters in
	let priority_values = map (\x -> if x.0 != 0 then x.0 else x.1 + 26) value_rough_letters in
	let priority_sum = u32.sum (priority_values) in

	(flags, lengths, offsets, reduced_letters, crushed_letters, value_rough_letters, priority_values, priority_sum)


entry part1 (file: []u8): u32 = 
	(part1_ file).8

entry part2 (file: []u8): u32 =
	(part2_ file).7
