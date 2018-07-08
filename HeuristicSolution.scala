import java.io._
import scala.collection.mutable._

final class Cell(val x:Int, val y:Int);

//this is heuristic approach based solution in combination with backtracking method.

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
   

   def getDegree(x:Int, y: Int, result: Array[Array[Int]]):Int = {
      var degree:Int = 0;

      for(i: Int <- 0 to 7){
            var xNext:Int = x + xDirection(i)
            var yNext:Int = y + yDirection(i)

            if(xNext >= 0 
               && xNext < N 
               && yNext >= 0 
               && yNext < N 
               && result(xNext)(yNext) == -1
            ) {
               degree+=1;
            }
               
         }

      return degree

   }

   def findRoute(dx: Int, dy: Int, result: Array[Array[Int]], moveNumber: Int): Boolean = {

   		if(moveNumber == N*N){
   			println("Pawn visited all the cells in the board");
   			return true;
   		}

         var minDegree: Int = 8;
         
         
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
               //check degree of each next valid reachable cell
               
               var degree:Int = getDegree(xNext, yNext, result)
   				if(degree < minDegree && degree> 0)
               {
                  minDegree = degree;
                  
               }
   			}		
   		}

         
         var minMoves:Map[Int, Cell] = Map();
         var pendingMoves: Map[Int, Cell] = Map();
         var index = 0;
         var pendingIndex = 0;


         // //creating array of all    
           for(i: Int <- 0 to 7){
            var xNext:Int = dx + xDirection(i)
            var yNext:Int = dy + yDirection(i)

            if(xNext >= 0 
               && xNext < N 
               && yNext >= 0 
               && yNext < N 
               && result(xNext)(yNext) == -1
            ) {
               //check degree of each next valid reachable cell
               var degree:Int = getDegree(xNext, yNext, result)
               if(degree == minDegree)
               {
                  minMoves.put(index, new Cell(xNext, yNext));
                  index+=1;                  
               }
               else{
                     pendingMoves.put(pendingIndex, new Cell(xNext, yNext));
                     pendingIndex+=1;    
               }

            }
         }   
         
         for (move <- minMoves) {
            val xMinNext = move._2.x;
            val yMinNext = move._2.y;

            result(xMinNext)(yMinNext) = moveNumber
            if(findRoute(xMinNext, yMinNext, result, moveNumber+1)){
               return true;
            }
            else{
               result(xMinNext)(yMinNext) = -1
            }  
         }
         
         for (move <- pendingMoves) {
            val xMinNext = move._2.x;
            val yMinNext = move._2.y;

            result(xMinNext)(yMinNext) = moveNumber
            if(findRoute(xMinNext, yMinNext, result, moveNumber+1)){
               return true;
            }
            else{
               result(xMinNext)(yMinNext) = -1
            }  
         }
        

   	  return false;
   }
}

object PawnMoveHeuristic {
   def main(args: Array[String]) {

   	// pass the starting position of pawn & chequeboard size to PawnMove class instance	
      val pt = new PawnMove(2, 2, 10);

      pt.solve();
   }
}