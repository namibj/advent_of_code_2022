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

let  part1_ (file: []u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let endian_worthness_map = correct_endian_worth_map flags lengths in
	let worthyness_adjusted_digit_values = map2 (\x y -> if (x == newline) then 0 else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map in 
	let digit_string_values = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values in
	let group_sums = sum_within_groups digit_string_values lengths in
	let max_sum = i32.maximum group_sums in
--= reduce_comm (i32.maximum) (i32.lowest) group_sums in
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





--		match (a < d, a < e, a < f, b < d, b < e, b < f, c < f)
--		case  ( true,  true,  true,  true,  true,  true, true) -> (d, e, f)
--		case  ( true,  true,  true,  true,  true,  true, true) -> (d, e, f)
--		case  ( true,  true,  true,  true,  true,  true, true) -> (d, e, f)
--		case  ( true,  true,  true,  true,  true,  true, true) -> (d, e, f)
--		let (a d) = if a > d then (a d) else (d a) in
--		let (d e) = if d > e then (d e) else (e d) in
--		let (d e) = if d > e then (d e) else (e d) in
--		let (d e) = if d > e then (d e) else (e d) in


entry part1 (file: []u8): u32 = 
	u32.i32 (part1_ file).8

entry part2 (file: []u8): u32 =
	let (a, b, c) = (max3 (part1_ file).7) in
	u32.i32 (a + b + c)
