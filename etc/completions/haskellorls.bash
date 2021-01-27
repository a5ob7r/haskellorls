_haskellorls()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $($(which haskellorls) "${CMDLINE[@]}") )
}

complete -o filenames -F _haskellorls haskellorls
