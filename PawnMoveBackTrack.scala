import java.io._

//back tracking solution for given problem 
//This is in-efficient solution as it keeps on checking all possible routes.
class PawnMove(val xStart: Int, val yStart: Int, val N: Int) {
   var x: Int = xStart
   var y: Int = yStart

   //initialize move directions array
   var xDirection = Array(3, 2, 0, -2, -3, -2, 0, 2);
   var yDirection = Array(0, 2, 3, 2, 0, -2, -3, -2);	
   

   def solve() {
   	
      // define & initialize the result array
       var result = Array.ofDim[Int](N,N) 

	   for(i:Int <- 0 to N-1){
	   		for(j:Int <- 0 to N-1){
	   			result(i)(j) = -1;
	   		}
	   }
	   //marking the starting moveNumber as 0 - 0th move
	   result(x)(y) = 0;

	   if(findRoute(x, y, result, 1)){
	   		println("pawn move directions: ")

	   		for(i:Int <- 0 to N-1){
		   		for(j:Int <- 0 to N-1){
		   			print(result(i)(j) + " ")
		   		}
		   		println()
	   		}
	   }
	   else
	   		println("pawn can not reach all cells by any route")
   }
   

   def findRoute(dx: Int, dy: Int, result: Array[Array[Int]], moveNumber: Int): Boolean = {

   		if(moveNumber == N*N ){
   			println("Pawn visited all the cells in the board");
   			return true;
   		}

   		//if these are valid co-ordinates
   		for(i: Int <- 0 to 7){
   			var xNext:Int = dx + xDirection(i)
   			var yNext:Int = dy + yDirection(i)

   			//checking validity of next co-ordinates
   			// And checking if cell is already visited
   			
   			if(xNext >= 0 
   				&& xNext < N 
   				&& yNext >= 0 
   				&& yNext < N 
   				&& result(xNext)(yNext) == -1
   			) {
   				result(xNext)(yNext) = moveNumber
	   			if(findRoute(xNext, yNext, result, moveNumber+1)){
	   				return true;
	   			}
	   			else{
	   				result(xNext)(yNext) = -1
	   			}	
   			}
   				
   		}
   	  return false;
   }
}

object BackTrackingSolution {
   def main(args: Array[String]) {

   	// pass the starting position of pawn & chequeboard size to PawnMove class instance	
      val pt = new PawnMove(2, 2, 6);

      pt.solve();
   }
}







// result(xNext)(yNext) = moveNumber
//                   if(findRoute(xNext, yNext, result, moveNumber+1)){
//                      return true;
//                   }
//                   else{
//                      result(xNext)(yNext) = -1
//                   }  