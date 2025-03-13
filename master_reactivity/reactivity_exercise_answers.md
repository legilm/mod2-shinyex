Answer of the Part 1

**1** - The difference between reactive() and eventReactive() in Shiny is that reactive() is a function that creates a reactive expression, while eventReactive() is a function that creates a reactive expression that only updates when an event occurs.

**2** - observe() differ from observeEvent() on the fact that observe() is a function that creates a reactive observer, so if any changes happens on the function, it will do an action. While observeEvent() is a function that creates a reactive observer that only updates when an specific event occurs.
example on observe(): is useful when you want to change the value of a variable when another variable changes, 
example on observeEvent() is useful when you want to change the value of a variable when a button is clicked.

**3** - reactiveValues() is necessary for some applications because it allows you to store reactive values that can be accessed and modified by multiple reactive expressions.
The function reactive() will not store the values and this can be a problem when you need to access the values in multiple reactive expressions.

**4** - When you use reactive content outside of a reactive context, it will brings an error, because shiny will not know how to handle the reactive content.

**5** - Inside on the reactive function, shiny will understand that every time that a input changes, the output will be updated. It's like a loop that will update the output everytime that the input changes.