%     Copyright (C) 2003 State of California, Department of 
%     Water Resources.
%     This program is licensed to you under the terms of the GNU General
%     Public License, version 2, as published by the Free Software
%     Foundation. More license details are available at the end of this file
function attr = createDSSAttr(yunits,ytype)
%
% Author:  Tawnly Pranger
%          Delta Modeling Section
%          California Department of Water Resources
% Created: September 4, 2003
%
% Creates a VISTA DataSetAttr class for RegularTimeSeries creation
%   yunits  - the RegularTimeSeries
%   ytype   - the location of the DSS file as a string
% Because this is used only for RegularTimeSeries creation in MATLAB,
% default values of DataType.REGULAR_TIME_SERIES for type, TIME for
% xunits and nothing for xtype
% Default for ytype is INST-VAL (instantaneous value); 
%             ytype may also be PER-AVER (period average)
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

if nargin < 2
    % Assign default value
    ytype = 'INST-VAL';
    %ytype = 'PER-AVER';
end
if nargin < 1
    error ('units must be defined');
end

if ~(isstr(yunits)) error('yunits must be a string'); end
if ~(isstr(ytype)) error('ytype must be a string'); end

type = DataType.REGULAR_TIME_SERIES;
xunits = 'TIME';
xtype = '';

attr = DataSetAttr(type,xunits,yunits,xtype,ytype);

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
