%     Copyright (C) 2003 State of California, Department of 
%     Water Resources.
%     This program is licensed to you under the terms of the GNU General
%     Public License, version 2, as published by the Free Software
%     Foundation. More license details are available at the end of this file
function array = createArrayFromDSS(file,path,timewindow,isDaily)
%
% Author:  Tawnly Pranger
%          Delta Modeling Section
%          California Department of Water Resources
% Created: September 4, 2003
%
% Creates a MATLAB array from a DSS file
%   file is the location of the DSS file as a string
%   path is the unique path of the data as a string
%   timewindow is the timewindow for the data to be retrieved from the DSS file
%     this is expressed as a string.  If blank, the entire length is returned
%   isDaily is a boolean to decide if the data should be converted to daily
%
% this function requires that the following files be included 
% in MATLAB's classpath.txt file (located in toolbox\local):
%       vista.jar       (located in vista\lib)
%       pd.jar          (located in vista\lib)
%       misc.jar        (located in vista\lib)
%       jython.jar      (located in vista\jython)
% The vista\lib directory must also be on your system path
import vista.time.*
import vista.set.*
import vista.db.dss.*

if (nargin < 4)
    % default behavior is to create daily data
    isDaily = 1;
end
if (nargin < 2)
    fprintf(['usage: createArrayFromDSS(file,path,[timewindow,[isDaily]])\n',...
             '       file       - name of DSS file\n',...
             '       path       - DSS pathname /a/b/c/d/e/f/ (individual path parts may be ommitted)\n',...
             '       timewindow - format: "ddMMMyyyy hhmm - ddMMMyyyy hhmm"\n',...
             '       isDaily    - (optional) - 0 = no change, 1 = time series will be converted to daily\n']);
    error('file, path, and timewindow parameters must be supplied');
end

if ~(isstr(file)) error('file must be a string'); end
if ~(isstr(path)) error('path must be a string'); end
if ~(isstr(timewindow) | isempty(timewindow)) error('timewindow must be a string'); end
if ~(isDaily == 0 | isDaily == 1) error('isDaily must be either 0 or 1'); end
if strfind(path,'/IR-')
  isRTS = 0;
else
  isRTS = 1;
end

tf = TimeFactory.getInstance;

f = java.io.File(file);
if (~(f.isFile))
    str = ['ERROR: File (',file,') could not be found\n'];
    fprintf(str);
    file
    array = [];
    return;
end
dssGroup = DSSUtil.createGroup('local',file);
% $$$ [pathPart,rem] = strtok(path,['/']);
% $$$ while ~(isempty(pathPart)) & strcmp(pathPart,'') == 0
% $$$     dssGroup.filterBy(1,PathnamePredicate(pathPart));
% $$$     [pathPart,rem] = strtok(rem,['/']);
% $$$ end   
ref = dssGroup.getDataReference(0);
data = ref.getData;
if isDaily
    data = SetUtils.createDaily(data);
end   
if ~(isempty(timewindow)) & strcmp(timewindow,'') == 0
    tw = tf.createTimeWindow(timewindow);
    data = data.createSlice(tw);
end    
if isRTS
  array = data.getYArray;
else
  % array_x is a array of DataSetElement
  % it has a method getXString() that will return time as a string
  % "01JAN2000 1233" and a method getY() that will return the value as a
  % double. 
  % E.g. array[0].getXString for time and array[0].getY for value
  array_x = javaMethod('__getslice__',data,0,data.size());
  array = cell(length(array_x),2);
  for i=1:length(array_x)
    array{i,1}=array_x(i).getXString;
    array{i,2}=array_x(i).getY;
  end
end

%     VISTA : A VISualization Tool and Analyzer. 
%     California Dept. of Water Resources
%     Division of Planning, Delta Modeling Section
%     1416 Ninth Street
%     Sacramento, CA 95814
%     (916)-653-5791
% 
%     Send bug reports to vista_bugs@water.ca.gov
% 
%     This program is licensed to you under the terms of the GNU General
%     Public License, version 2, as published by the Free Software
%     Foundation.
% 
%     You should have received a copy of the GNU General Public License
%     along with this program; if not, contact the Delta Modeling Section Chief, below,
%     or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
%     02139, USA.
% 
%     THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
%     DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
%     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%     IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%     PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
%     DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
%     ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
%     OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
%     BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
%     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%     DAMAGE.
% 
%     For more information about VISTA, contact:
% 
%     Section Chief
%     California Dept. of Water Resources
%     Division of Planning, Delta Modeling Section
%     1416 Ninth Street
%     Sacramento, CA  95814
%     916-653-5791
% 
%     or see our home page: http://modeling.water.ca.gov/
% 
%     Send bug reports to vista_bugs@water.ca.gov
