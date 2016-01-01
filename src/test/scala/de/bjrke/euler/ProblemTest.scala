package de.bjrke.euler

import org.clapper.classutil.ClassFinder
import org.testng.annotations.{DataProvider, Test}
import org.testng.Assert._

/**
 * tests all problems
 *
 * Created by bjrke on 01.01.16.
 */
class ProblemTest {

  @DataProvider def problems = ClassFinder
    .concreteSubclasses(
      classOf[Problem[_]].getCanonicalName, ClassFinder().getClasses.iterator
    )
    .map{ ci => Array[Any]( java.lang.Class.forName( ci.name ).newInstance ) }
    .toArray

  @Test(dataProvider = "problems") def testProblems( problem : Problem[_] ) =
    assertEquals(problem.apply, problem.result)

}
