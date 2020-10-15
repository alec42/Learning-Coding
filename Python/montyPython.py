import time

name = input("Stop! What... is your name? ")
# quest = input("What... is your quest {}? ".format(name))
quest = input("What... is your quest? ")
if quest == "To seek the Holy Grail." :
	print("Very well... I see you are a fan of Monty Python !")
	time.sleep(1)
velocity = input("What... is the air-speed velocity of an unladen swallow? ")
print("Huh? I-- I don't know that.")
time.sleep(1)
print("Auuuuuuuugh!")
print(f"You are {name}!")
print("say what what in the {0:6} aaa".format(name))
quqst = int(quest)
print(f'say  {quqst:8.2e} what what {quqst:^8.2f} in the {quqst:8.2f} aaa')
input()