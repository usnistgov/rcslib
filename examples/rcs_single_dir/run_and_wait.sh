#! /bin/sh


echo "Running $*";
echo "with evironment:"
set
echo ""
echo "<end environment>"
echo ""

$*;

echo "Finished $*";
echo "with evironment:"
set
echo ""
echo "<end environment>"
echo ""


echo "Press enter to close."
read foo
