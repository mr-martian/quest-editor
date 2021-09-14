export module containers

import ranges // provides trait range_of, trait iterator_for, trait sentinel

export enum order(equal=0, less=-1, greater=1)

// implement trait array for builtin sized arrays
export fn begin(self : &mut? [Size]) {
	return array_iterator(self, 0)
}
export fn end(self : &mut? [Size]) {
	return array_iterator(self, Size)
}

// builtin sized arrays support this trait
export trait array(Data : type) {
	implements range_of(Data);
	
	fn size(self : &array) -> i64;
	fn operator[](self : &array, i : i64) -> &Data;
	fn operator[](self : &mut array, i : i64) -> &mut? Data;
	
	fn operator[:](self : &mut? array) -> range_of(Data);
	fn operator[:](self : &mut? array, lb : i64) -> range_of(Data);
	fn operator[:](self : &mut? array, lb : None, rb : i64) -> range_of(Data);
	fn operator[:](self : &mut? array, lb : i64, rb : i64) -> range_of(Data);
	
	fn begin(self : &array) -> iterator_for(Data);
	fn begin(self : &mut array) -> iterator_for(mut? Data);
	fn end(self : &mut? array) -> sentinel;
	
	fn operator==(lhs : &array, rhs : &array) -> bool;
	fn operator<=>(lhs : &array, rhs : &array) -> order;
}

export trait list(Data : type) {
	// requires everything in trait array as well
	implements array(Data);
	
	fn new() -> list;
	fn new(other : &list) -> list;
	fn new(arr :*[Size : i64]Data) -> list;
	
	// these functions have no required return type
	fn push_back(self : &mut list, val : Data);
	fn append(self : &mut list, r : range_of(Data));
	fn pop_back(self : &mut list) -> Data;
}

export fn @vector(#Data : type) -> type { // template implemented by macro
	return #struct Vector { // return a type which calls itself Vector
		// completely unoptimized dynamic array implementation
		let data : [dyn]Data {private mut}
		
		defer array(Data) to data;
		
		// assertions (no effect besides trait checking)
		implements list(Data);
		
		public fn new() -> Vector = default;
		public fn new(other : &Vector) -> Vector = default;
		public fn new(arr :*[Size : i64]Data) -> Vector {
			return Vector{data : arr};
		}
		
		public fn push_back(self : &mut Vector, val : Data) {
			self.data := {self.data..., val}
		}
		public fn append(self : &mut Vector, r : range_of(Data)) {
			self.data := {self.data..., r...}
		}
		public fn pop_back(self : &mut Vector) -> Data {
			let back = self[self.size - 1].move
			let tmp : [dyn]Data [self.size - 1]
			for (let i : range(self.size - 1)) {
				tmp = self.data[i].move
			}
			self.data := tmp.move
			return back
		}
	}
}

export struct string {
	let chars : vector(char8) {public mut}
	// defer a trait to a member, instead of using inheritance
	defer list(char8) to chars;
}

export fn operator+(lhs : string, rhs : &string) -> string {
	lhs.append(rhs)
	return lhs
}

export fn icompare(lhs : &string, rhs : &string) -> order {
	for (let i : range(min(lhs.size, rhs.size))) {
		// this is not unicode-aware
		let o = lhs[i].tolower <=> rhs[i].tolower
		if (o != order::equal) { return o }
	}
	return order::equal
}


