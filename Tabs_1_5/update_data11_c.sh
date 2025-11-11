## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs5
sudo mkdir /srv/shiny-server/Abs5


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs5

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs5/ /srv/shiny-server/Abs5 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs5

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
