package com.xmas.challenge.twenty19

object Main extends App {

    println(Day1.calculateDefaultFuel)

    println(Day2.executeDefaultIntCode)
    println(IntCodeComputer.execute(IntCodeComputer.initialize(12,2)))

    println(IntCodeComputer.findInput(9581917))
    println(IntCodeComputer.execute(IntCodeComputer.getDefaultMemory))
    println(IntCodeComputer.execute(IntCodeComputer.initialize(25,5)))
    println(IntCodeComputer.findInput(19690720))


//    println("Shortest Distance: " + Day3.calculateShortestDistance(Day3.wireA,Day3.wireB))
//    println("Fewest Steps: " + Day3.calculateFewestSteps(Day3.wireA,Day3.wireB))


    println(s"Num passwords: ${Day4.passwordCount(Day4.defaultStart,Day4.defaultEnd)}")
}
