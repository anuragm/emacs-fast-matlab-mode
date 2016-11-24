function getCompletions(substring)
%Echos to display the number of completion candidates for string sub-string.

% The class com.mathworks.jni.MatlabMCR is a JAVA bridge to Matlab runtime. We use it to
% fetch completion candidates. We keep this persistent for performance reasons.
% This code only works in MATLAB R2014b or above.

persistent mcr_bridge;
if isempty(mcr_bridge)
    mcr_bridge = com.mathworks.jmi.MatlabMCR;
end

completionCandidates = mcr_bridge.mtFindAllTabCompletions(substring,length(substring),0);
if isempty(completionCandidates)
    return;
end

for ii=1:length(completionCandidates)
    fprintf('%s\n',char(completionCandidates(ii)));
end

end
