% 1. read an xml file and store it into z :
z = xmltools('c172p.xml')

% 2. write z into the file :
xmltools(z,'test1.xml')


% 3. returns only subset of z child which name is tag-name :
% xmltools(z,'get','tag-name')

% and now:

% 4. returns only subset of z child which attrib is attrib-name :
% xmltools(z,'get-attrib', 'attrib-name')

% 5. returns only attribs/value or children of subset of z child which name is tag-name :
% xmltools(z,'get','tag-name', 'attribs'|'value'|'child');
