
rm test/*.pdf

ls test/*.webrexp | % {
    $o = $_
    echo "========================================"
    echo $o.name
    echo "========================================"
    dist/build/Webrexp/webrexp --dot -f $o.fullname | out-file -encoding ascii $($o.fullname + ".dot")
    dot -Tpdf -o $($o.fullname + ".pdf") $($o.fullname + ".dot")
}
