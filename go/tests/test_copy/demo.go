package main

import (
	"fmt"
)

type Person struct {
	Name    string
	Age     int
	Friends []string
	Memory  map[string][]string
}

// This shows that slices and maps are not copied and if they are changed\
// they change the original object

func main() {
	alice1 := Person{"Alice", 30, []string{"a", "b"}, map[string][]string{}}
	alice2 := alice1
	alice3 := alice1

	alice2.Friends = append(alice2.Friends, "c") // use of append creates a copy
	fmt.Println("alice1:", alice1.Friends)

	alice2.Memory["yesterday"] = []string{"nice weather", "dog ran in park"}
	fmt.Println("alice1:", alice1.Memory)
	
	//fmt.Println(alice1 == alice2)   // => fail, as slices cannot be compared
	fmt.Println(&alice1 == &alice2) // => false, they have different addresses

	alice2.Age += 10
	//fmt.Println(alice1 == alice2)

	// alice3.Friends = append(alice3.Friends, "1")
	alice3.Friends = make([]string, len(alice2.Friends))
	fmt.Println("alice3.Friends:", alice3.Friends)
	var res int = copy(alice3.Friends, alice2.Friends)
	fmt.Println("copy(alice3.Friends, alice2.Friends) -> alice2.Friends:", alice2.Friends)
	fmt.Println("alice3.Friends:", alice3.Friends, "res:", res)
}
