function fish_prompt
  echo -s \
    (set_color blue) (prompt_pwd) \
    (set_color cyan) (__fish_git_prompt) \
    (set_color green) (__kube_prompt) \
    (set_color normal) " > "
end
