package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
    	var result = Set[Double]()
    	val d = computeDelta(a, b, c)()
    	
    	if(a() != 0){
	    	if(d > 0){
	    		result += (- b() + math.sqrt(d)) / 2 / a()
	    		result += (- b() - math.sqrt(d)) / 2 / a()
	    	} else if(d == 0){
	    		result += - b() / 2 / a()
	    	}
	    }

    	result
    }
  }
}
