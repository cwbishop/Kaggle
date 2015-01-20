for i=1:numel(subjects)
    d(i) = pop_loaderp('filepath', fullfile('D:\GitHub\Kaggle\P300_Speller', subjects{i}, 'analysis'), 'filename', [subjects{i} '-' erp_label '.erp']);
end