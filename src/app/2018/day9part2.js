const numElves = 416;
const lastMarble = 7197500;

let marbleArray = [0],
    scores = new Array(numElves),
    currentIndex = 0,
    currentElf = 1

scores.fill(0)

for (let marble = 1; marble <= lastMarble; marble++) {
    let newIndex;
    
    if (marble % 23 == 0) {
        if (currentIndex < 7) {
            currentIndex = marbleArray.length - (7 - currentIndex)
        } else {
            currentIndex = currentIndex - 7
        }
        
        scores[currentElf] += marble + marbleArray[currentIndex]
        
        marbleArray.splice(currentIndex, 1)
    } else {
        if (marbleArray.length - currentIndex == 2) {
            currentIndex += 2
            marbleArray[currentIndex] = marble
        } else if (marbleArray.length - currentIndex == 1) {
            currentIndex = 1
            marbleArray.splice(currentIndex, 0, marble)
        } else {
            currentIndex += 2
            marbleArray.splice(currentIndex, 0, marble)
        }
    }    
    currentElf = currentElf == numElves ? 1 : currentElf + 1;
}

console.log(scores.reduce((max, val) => val > max ? val : max, 0))