class Cat(val name: String, val color: String, val food: String) {
  def meow(firstName: String = "Johnny", lastName: String): String = {
    "Meow " + firstName + " " + lastName
  }
}

// val oswald = new Cat("Oswald", "Black", "Milk")
// val henderson = new Cat("Henderson", "Ginger", "Chips")
//
// oswald.meow(henderson.color, henderson.food)
// henderson.meow(lastName = oswald.name)