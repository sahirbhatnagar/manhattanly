# configure your name and email if you have not done so
git config --global user.email "sahir.bhatnagar@gmail.com"
git config --global user.name "sahirbhatnagar"


# clone the repository to the book-output directory
git clone -b gh-pages \
  https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
  book-output
cd book-output
git rm -rf *
cp -r ../_book/* ./
git add --all *
git commit -m "Update the complete version of the vignette"
git push -q origin gh-pages