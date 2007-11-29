/*
    Copyright (C) 1998 State of California, Department of Water
    Resources.

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/

package DWR.CSDP;
public class ResizableBooleanArray{
public ResizableBooleanArray(){
  numberOfResizeRequests = 0;
  resizeAccelerationStep = 5;
  setResizeStep(INITIAL_RESIZE_STEP);
  setArrayLength(INITIAL_ARRAY_LENGTH);
  initializeArray();
}
//constructor
public ResizableBooleanArray(int initialSize){
  numberOfResizeRequests = 0;
  resizeAccelerationStep = 5;
  setResizeStep(INITIAL_RESIZE_STEP);
  setArrayLength(initialSize);
  initializeArray();
}
public ResizableBooleanArray(int initialSize, int resizeStepSize){
  numberOfResizeRequests = 0;
  resizeAccelerationStep = 5;
  setResizeStep(resizeStepSize);
  setArrayLength(initialSize);
  initializeArray();
}
//copy constructor
public ResizableBooleanArray(ResizableBooleanArray rhs){
  numberOfResizeRequests = 0;
  resizeAccelerationStep = 5;
  resizeStep = rhs.resizeStep;
  arrayLength = rhs.arrayLength;
  array = new Boolean[arrayLength];
  System.arraycopy(rhs.array,0,this.array,0,arrayLength);
}

// return element for read without checks
public Boolean get(int index){
  if(index < 0 || index >= arrayLength)
    throw new ArrayIndexOutOfBoundsException("Illegal access to ResizableBooleanArray element");
  else 
    return(array[index]);
}

// return element for read/write: put value
public void put(int index, Boolean value){
  if(index >=0 && index < arrayLength)
    array[index] = value;
  else if (index >= arrayLength) {
    this.resize();
    this.put(index, value);
  }
  else
    throw new ArrayIndexOutOfBoundsException("Illegal access to ResizableBooleanArray element");
}
// return element for read/write: put value
public void put(int index, boolean value){
  put(index, new Boolean(value));
}
// return element for read/write: put object, store as value
public void put(int index, Object object){
  if(index >=0 && index < arrayLength)
    array[index] = (Boolean)object;
  else if (index >= arrayLength) {
    this.resize();
    this.put(index, (Boolean)object);
  }
  else
    throw new ArrayIndexOutOfBoundsException("Illegal access to ResizableBooleanArray element");
}
// request for length of array
public int getSize() {
  return arrayLength;
}

// resize array
public void resizeTo(int newSize){
  Boolean [] tmpArray;
  tmpArray = new Boolean[newSize];
  if(arrayLength < newSize){
    System.arraycopy(array,0,tmpArray,0,arrayLength);
  }
  else{
    System.arraycopy(array,0,tmpArray,0,newSize);
  }
  arrayLength = newSize;
  array = tmpArray;
}

// request for resizing array
public void resize(){
  numberOfResizeRequests++;
  if (numberOfResizeRequests > 5){
    resizeStep *= resizeAccelerationStep;
    numberOfResizeRequests = 0;
  }
  Boolean [] tmpArray;
  tmpArray = new Boolean[arrayLength+resizeStep];
  System.arraycopy(array,0,tmpArray,0,arrayLength);
  arrayLength = arrayLength + resizeStep;
  array = tmpArray;
}
//set array length after checks
protected void setArrayLength(int length){
  if (length <= 0) length = INITIAL_ARRAY_LENGTH;
  arrayLength=length;
}

//set resize step after checks
public void setResizeStep(int size){
  if (size <= 0) size = INITIAL_RESIZE_STEP;
  resizeStep=size;
}
protected void initializeArray(){
  array = new Boolean[arrayLength];
}

  ///////////
  // size of change of arrayLength on resize request
  protected int resizeStep;

  ///////////
  // if too many resize requests are made the resize parameter
  // is increased by a multiple equal to this variable
  protected int resizeAccelerationStep;

  ///////////
  // length of array
 protected int arrayLength;

  ///////////
  // keeps track of number of resize requests
 protected int numberOfResizeRequests;
 ///////////
 // array containing data
 protected Boolean[] array;

public static final int INITIAL_RESIZE_STEP = 20;
public static final int  INITIAL_ARRAY_LENGTH = 10;
}
