const searchFor = "236021";

let recipes = [3,7],
    elves = [0,1],
    done = false,
    numRecipes = 2

debugger;
while (!done) {
    if (numRecipes > 3000000) {
        let string = recipes.join('');
        console.log(string.indexOf(searchFor));
        done = true;
        continue;
    }

    let newRecipe = recipes[elves[0]] + recipes[elves[1]];
    if (newRecipe >= 10) {
        recipes.push(1);
        newRecipe = newRecipe - 10;
        numRecipes++;
    }
    recipes.push(newRecipe);

    numRecipes++;

    elves[0] = (elves[0] + recipes[elves[0]] + 1) % numRecipes;
    elves[1] = (elves[1] + recipes[elves[1]] + 1) % numRecipes;
}
