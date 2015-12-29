package com.gmail.hancury.fpsc

import scala.Stream

/**
 * @author Curycu
 */


/*
 * data: input or output
 * program: input => process
 * process: (state, program, output)
 * driver: (process, input) => output
 */


/*
 * program: logic for how to change input & how to make next process
 * process: state + program + output
 * driver: give notions(how to action) to each states
 */


//sealed trait Process[I, O]

//state
case class Halt[I, O]() extends Process[I, O]
//state + program
case class Await[I, O](program: Option[I] => Process[I, O]) extends Process[I, O]
//state + output + next process
case class Emit[I, O](output: O, process: Process[I, O] = Halt[I, O]) extends Process[I, O]

object Driver {

  //handle Process + Input
  @annotation.tailrec
  def driver[I, O](process: Process[I, O], is: Stream[I], os: Stream[O] = Stream()): Stream[O] 
  = process match {
    case Halt() => os.reverse //State: Halt -> stop driver
    case Await(program) => is match {
      case h #:: t => driver(program(Some(h)), t, os) //State: Await + Data: remain -> go to Emit
      case empty   => driver(program(None), empty, os) //State: Await + Data: empty -> go to Halt
    }
    case Emit(o, process) => driver(process, is, o #:: os) //State: Emit -> save data + go to Await
  }
  
  def fileDriver[O](proc: Process[String, O], is: java.io.File, os: Stream[O] = Stream()): Stream[O] 
  = {
    @annotation.tailrec
    def go(proc: Process[String, O], si: Iterator[String], os: Stream[O]): Stream[O]
    = proc match {
      case Halt() => os.reverse
      case Await(prog) => 
        if(si.hasNext){ 
          go(prog(Some(si.next)), si, os) 
        }else{ 
          go(prog(None), si, os) 
        } 
      case Emit(h, t) => go(t, si, h #:: os)
    }
    
    val src = io.Source.fromFile(is)
    try go(proc, src.getLines, os)
    finally src.close
  }
  
}

/*
 * where each step data are stored?
 * -> Emit(output, process) == h #:: driver(process, input). save output by chaining
 * 
 * so... then every program(Some(h)) should be equal to Emit for saving output
 * -> True, see [Process.lift]
 * 
 * (State: Await + Input: empty) does not stop driver immediately just loop with program(None) 
 * so every programs should have empty input case handling -> go Halt
 * -> True, empty input can be handled by driver or program. see [Driver.driverOne] and [Process.lift]
 */

object Process {

  //guarantee operation only once by process
  def liftOne[I, O](f: I => O): Process[I, O] //convert function to Await(state + program)
  = Await[I, O] {
    //program
    case Some(i) => Emit(f(i), Halt[I,O])
    case None    => Halt()
  }
  
  def filterOne[I](f: I => Boolean): Process[I, I] 
  = Await[I, I] {
    case Some(i) if f(i) => Emit(i)
    case _               => Halt()
  }
    
  def lift[I, O](f: I => O): Process[I, O] //convert function to Await(state + program)
  = liftOne(f).repeat
  
  def filter[I](f: I => Boolean): Process[I, I] 
  = filterOne(f).repeat
    
  private def loop[I, O, S](s1: S)(f: (I, S) => (O, S)): Process[I, O] 
  = Await[I, O] {
    case Some(i) => {
      val (o, s2) = f(i, s1)
      Emit(o, loop(s2)(f))
    }
    case None => Halt()
  }
  
  def count[I]: Process[I, Int] 
  = loop(0)((i: I, s: Int) => (s+1, s+1))
  
  def sum: Process[Int, Int]
  = loop(0)((i: Int, s: Int) => (i+s, i+s))
  
  def mean: Process[Double, Double]
  = loop((0.0, 1.0)){
    (acc, state) => state match {
      case (sum, count) => ((acc+sum)/count, (acc+sum, count+1))
    }
  }
  
  def exists[I](f: I => Boolean): Process[I, Boolean] 
  = lift(f) |> loop(false)((b, s) => (s || b, s || b))
}

/*
 * lift with Some, always yield Emit 
 * and lift with None, always yield Halt
 * 
 * liftOne just make a Emit or Halt -> no loop
 * so, there is only one operation
 * for repeating, new [def repeat] is needed 
 * and to make it easy to use, 
 * [def repeat] would be better to be a method of trait Process (method chaining)
 */

sealed trait Process[I, O] {

 /*[def repeat] travel path
  * 
  * Await -> Emit -> Halt -> Await
  * 
  * Await -> Emit(o, Halt) -> Emit(o, Await) -> Emit(o, Emit(o, Halt)) -> ...
  */
  def repeat(n: Int, dec: Int = 1): Process[I, O] = { //since lift(f) is Await(Emit(f(i)) or Halt()), repeat start with Await
    
    def go(proc: Process[I, O], n: Int): Process[I, O] 
    = proc match {
      case Halt() => go(this, n) //forever looping?? -> False, forever looping only when start with Halt().repeat
      case Await(prog) => Await {
        case Some(i) if(n>0) => go(prog(Some(i)), n) //loop -> Emit(f(i))
        case _    => prog(None) //empty input: prog(None) -> case match of lift -> Halt() -> The End
      }
      case Emit(o, proc2) => Emit(o, go(proc2, n - dec)) //output chaining: o #:: o #:: ...
    }
    
    go(this, n)
    //Await -> Emit(f(i)) -> Halt() -> Await -> Emit(f(i))
    //Await -> Halt()
  }
  
  def repeat: Process[I, O] = repeat(1, 0)
  
  /*
   * function composition: f compose g = f(g)
   * just previous output -> next input
   * 
   * but process has variety states
   */
  
  def |>[O2](proc: Process[O, O2]): Process[I, O2] 
  = proc match {
    case Halt() => Halt()
    case Emit(h,t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Halt() => Halt() |> f(None) 
      case Emit(h,t) => t |> f(Some(h)) 
      case Await(g) => Await((i: Option[I]) => g(i) |> proc)
    }
  }
  
  def map[O2](f: O => O2): Process[I,O2] = this |> Process.lift(f)
  
  def ++(proc2: => Process[I,O]): Process[I,O] = this match {
    case Halt() => proc2
    case Emit(h, t) => Emit(h, t ++ proc2)
    case Await(prog1) => Await(prog1 andThen (_ ++ proc2))
  }
  
  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(prog) => Await(prog andThen (_.flatMap(f)))
  }
}

object DriverTest extends App {

  //Stream -> Stream
  val input1 = Stream("a","b","c")
  val input2 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
	val input3 = Stream(1.0, 2.0, 3.0, 4.0, 5.0)
	val input4 = Stream.continually(1)

  val lift1 = Process.liftOne { (x: Int) => 10 * x }
  val lift2 = Process.liftOne { (x: Int) => 10 * x }.repeat(2)
  val liftAll = Process.lift { (x: Int) => 10 * x }
  val even2 = Process.filterOne { (x: Int) => x % 2 == 0 }.repeat(2)
	val evenAll = Process.filter { (x: Int) => x % 2 == 0 }
  val procPipe = Process.sum |> evenAll |> Process.count[Int]

  val output = Driver.driver(procPipe, input2)
  println(output.toList)
  
  //File -> Stream
  val check = Process.count[String] |> Process.exists(_ > 19)
  val file = new java.io.File("test.txt")
  val fileCheck = Driver.fileDriver(check, file)
  println(fileCheck.toList)
  
  //File -> File
  def toCelsius(fahrenheit: Double): Double
  = (5.0 / 9.0) * (fahrenheit - 32.0)
  
}
 