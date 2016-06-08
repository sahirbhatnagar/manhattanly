# configure your name and email if you have not done so
git config --global user.email "sahir.bhatnagar@gmail.com"
git config --global user.name "sahirbhatnagar"


# clone the repository to the book-output directory
git clone -b gh-pages \
  https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git \
  book-output
cd book-output
cp -r ../_book/* ./
rm -rf _bookdown_files
git add *
git commit -m "Update the complete version of the vignette"
git push origin gh-pages