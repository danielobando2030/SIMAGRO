## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs3
sudo mkdir /srv/shiny-server/Abs3


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs3

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs3/ /srv/shiny-server/Abs3 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs3

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
