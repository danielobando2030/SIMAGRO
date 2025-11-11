## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs2
sudo mkdir /srv/shiny-server/Abs2


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs2

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs2/ /srv/shiny-server/Abs2 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs2

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
