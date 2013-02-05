%     Copyright (C) 2003 State of California, Department of 
%     Water Resources.
%     This program is licensed to you under the terms of the GNU General
%     Public License, version 2, as published by the Free Software
%     Foundation. More license details are available at the end of this file
function writeArrayToDSS(array,file,path,startTime,interval,attr)
%
% Author:  Tawnly Pranger
%          Delta Modeling Section
%          California Department of Water Resources
% Created: September 4, 2003
%
% Writes a MATLAB array converted to a RegularTimeSeries to a DSS file
%   array     - the values of the times series as a numeric array
%   file      - the location of the DSS file as a string
%   path      - the unique path of the data as a string
%   startTime - VISTA formatted starting time for the data as a string
%   interval  - time interval of the data as a string
%   attr      - attributes - created using the function createDSSAttr
%               if using default of INST-VAL for type, units as a string 
%               can be provided instead
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

if nargin < 6
    fprintf(['usage: writeArrayToDSS(array,file,path,startTime,interval,attr)\n',...
             '         array     - the values of the times series as a numeric array\n',...
             '         file      - the location of the DSS file as a string\n',...
             '         path      - the unique path of the data as a string\n',...
             '         startTime - VISTA formatted starting time for the data as a string\n',...
             '         interval  - time interval of the data as a string\n',...
             '         attr      - attributes - created using the function createDSSAttr\n',...
             '                     if using default of INST-VAL for type, units as a string\n',...
             '                     can be provided instead']);        
    error('Must supply all parameters');
end

if ~(isnumeric(array)) error('data must be a numeric array'); end
if ~(isstr(file)) error('file must be a string'); end
if ~(isstr(path)) error('path must be a string'); end
if ~(isstr(startTime)) error('startTime must be a string'); end
if ~(isstr(interval)) error('interval must be a string'); end
if ~(isa(attr,'DataSetAttr') | isstr(attr)) error('attr must be of VISTA class DataSetAttr.  Use function createDSSAttr'); end

if isstr(attr)
    % if units are passed in (default of INST-VAL for type) create
    % DataSetAttr
    attr = createDSSAttr(attr);
end

flags = zeros(size(array));

rts = RegularTimeSeries('rts',startTime,interval,array,flags,attr);

DSSUtil.writeData(file,path,rts);

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
